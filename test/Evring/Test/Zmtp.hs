{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Property tests for ZMTP 3.x protocol parser.
--
-- These tests exercise the reset-on-ambiguity discipline that is
-- proven correct in Cornell.Zmtp.
module Evring.Test.Zmtp
  ( tests
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( testProperty
  , Arbitrary(arbitrary)
  , Gen
  , Property
  , (===)
  , (.&&.)
  , choose
  , elements
  )

import Evring.Zmtp
  ( Greeting(greetingVersionMajor, greetingVersionMinor)
  , Mechanism(MechNull, MechPlain, MechCurve)
  , FrameHeader(frameSize, frameHasMore, frameIsLong, frameIsCommand)
  , ZmtpParseResult(Ok, Incomplete, Ambiguous)
  , AmbiguityReason(InvalidSignature, UnsupportedVersion, ReservedFlagsSet, FrameTooLarge, MechanismMismatch)
  , ZmtpState(zmtpPhase, zmtpBuffer)
  , ConnPhase(PhaseAwaitGreeting)
  , greetingSize
  , parseGreeting
  , parseFrameHeader
  , hasReservedBits
  , initialZmtpState
  )

-- ═══════════════════════════════════════════════════════════════════════════════
-- GENERATORS
-- ═══════════════════════════════════════════════════════════════════════════════

-- | Generate a valid ZMTP greeting (64 bytes)
_genValidGreeting :: Gen ByteString
_genValidGreeting = do
  -- Signature bytes
  let sig = BS.pack [0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x7F]
  -- Version (3.x)
  major <- elements [3, 4, 5]  -- ZMTP 3+
  minor <- choose (0, 10)
  let version = BS.pack [major, minor]
  -- Mechanism (NULL, PLAIN, or CURVE)
  mech <- elements ["NULL\0", "PLAIN\0", "CURVE\0"]
  let mechPadded = mech <> BS.replicate (20 - BS.length mech) 0
  -- as-server flag
  asServer <- elements [0x00, 0x01]
  -- Filler (31 bytes)
  let filler = BS.replicate 31 0x00
  pure $ sig <> version <> mechPadded <> BS.singleton asServer <> filler

-- | Generate an invalid greeting (wrong signature)
_genInvalidSignatureGreeting :: Gen ByteString
_genInvalidSignatureGreeting = do
  sig0 <- choose (0x00, 0xFE)  -- Not 0xFF
  let rest = BS.replicate 63 0x00
  pure $ BS.cons sig0 rest

-- | Generate valid frame header flags (reserved bits clear)
_genValidFlags :: Gen Word8
_genValidFlags = do
  more <- elements [0x00, 0x01]
  long <- elements [0x00, 0x02]
  cmd  <- elements [0x00, 0x04]
  pure (more + long + cmd)

-- | Generate invalid frame header flags (reserved bits set)
_genInvalidFlags :: Gen Word8
_genInvalidFlags = do
  base <- _genValidFlags
  reserved <- choose (0x08, 0xF8)  -- Set some reserved bits
  pure (base + reserved)

-- ═══════════════════════════════════════════════════════════════════════════════
-- PROPERTY TESTS - GREETING
-- ═══════════════════════════════════════════════════════════════════════════════

-- | Property: Valid greeting parses successfully
-- Lean theorem: parseGreeting_deterministic
prop_validGreetingParses :: Property
prop_validGreetingParses =
  let greeting = BS.pack $ 
        [0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x7F]  -- signature
        ++ [0x03, 0x01]  -- version 3.1
        ++ [0x4E, 0x55, 0x4C, 0x4C, 0x00] ++ replicate 15 0x00  -- NULL mechanism
        ++ [0x00]  -- as-server
        ++ replicate 31 0x00  -- filler
  in case parseGreeting greeting of
    Ok g _ -> greetingVersionMajor g === 3 .&&. greetingVersionMinor g === 1
    _ -> False === True  -- Should parse

-- | Property: Greeting exactly 64 bytes
-- Lean theorem: greeting_exactly_64
prop_greetingExactly64 :: Property
prop_greetingExactly64 =
  greetingSize === 64

-- | Property: Incomplete greeting returns Incomplete
prop_incompleteGreeting :: Property
prop_incompleteGreeting =
  let partial = BS.replicate 32 0xFF  -- Only 32 bytes
  in case parseGreeting partial of
    Incomplete n -> n === (64 - 32)
    _ -> False === True

-- | Property: Invalid signature triggers ambiguity
-- Lean: if sig0 != signatureByte0 then .ambiguous (.invalidSignature ...)
prop_invalidSignatureAmbiguous :: Property
prop_invalidSignatureAmbiguous =
  let badGreeting = BS.pack $ [0x00] ++ replicate 63 0x00  -- Wrong sig0
  in case parseGreeting badGreeting of
    Ambiguous (InvalidSignature _ _) -> True === True
    _ -> False === True

-- | Property: Old version triggers ambiguity
-- Lean: if major < 3 then .ambiguous (.unsupportedVersion major minor)
prop_oldVersionAmbiguous :: Property
prop_oldVersionAmbiguous =
  let oldGreeting = BS.pack $
        [0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x7F]  -- valid sig
        ++ [0x02, 0x00]  -- version 2.0 (too old)
        ++ replicate 52 0x00
  in case parseGreeting oldGreeting of
    Ambiguous (UnsupportedVersion 2 0) -> True === True
    _ -> False === True

-- | Property: Unknown mechanism triggers ambiguity
-- Lean: | none => .ambiguous (.mechanismMismatch ...)
prop_unknownMechanismAmbiguous :: Property
prop_unknownMechanismAmbiguous =
  let badGreeting = BS.pack $
        [0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x7F]  -- valid sig
        ++ [0x03, 0x01]  -- version 3.1
        ++ [0x58, 0x58, 0x58, 0x58, 0x00] ++ replicate 15 0x00  -- "XXXX" unknown
        ++ [0x00]  -- as-server
        ++ replicate 31 0x00
  in case parseGreeting badGreeting of
    Ambiguous (MechanismMismatch _) -> True === True
    _ -> False === True

-- ═══════════════════════════════════════════════════════════════════════════════
-- PROPERTY TESTS - FRAME HEADER
-- ═══════════════════════════════════════════════════════════════════════════════

-- | Property: Reserved flags trigger ambiguity
-- Lean theorem: flags_unambiguous (reserved bits → ambiguous)
prop_reservedFlagsAmbiguous :: Word8 -> Property
prop_reservedFlagsAmbiguous flags =
  let hasReserved = hasReservedBits flags
      input = BS.pack [flags, 0x00]  -- flags + size
  in if hasReserved
     then case parseFrameHeader input of
       Ambiguous (ReservedFlagsSet _) -> True === True
       _ -> False === True
     else True === True  -- Valid flags, any result ok

-- | Property: Short frame header parses correctly
prop_shortFrameHeader :: Property
prop_shortFrameHeader =
  let input = BS.pack [0x00, 0x10]  -- flags=0, size=16
  in case parseFrameHeader input of
    Ok hdr _ -> 
      frameSize hdr === 16
      .&&. frameHasMore hdr === False
      .&&. frameIsLong hdr === False
      .&&. frameIsCommand hdr === False
    _ -> False === True

-- | Property: Long frame header needs 9 bytes
prop_longFrameHeaderIncomplete :: Property
prop_longFrameHeaderIncomplete =
  let input = BS.pack [0x02, 0x00, 0x00]  -- LONG flag, only 3 bytes total
  in case parseFrameHeader input of
    Incomplete n -> n === (9 - 3)
    _ -> False === True

-- | Property: Long frame header parses correctly
prop_longFrameHeader :: Property
prop_longFrameHeader =
  let input = BS.pack [0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00]  -- 256 bytes
  in case parseFrameHeader input of
    Ok hdr _ ->
      frameSize hdr === 256
      .&&. frameIsLong hdr === True
    _ -> False === True

-- | Property: Frame too large triggers ambiguity
-- Lean: if size64 > maxFrameSize.toUInt64 then .ambiguous (.frameTooLarge size64)
prop_frameTooLargeAmbiguous :: Property
prop_frameTooLargeAmbiguous =
  let hugeSize = BS.pack [0x02, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]  -- > 256MB
  in case parseFrameHeader hugeSize of
    Ambiguous (FrameTooLarge _) -> True === True
    _ -> False === True

-- ═══════════════════════════════════════════════════════════════════════════════
-- PROPERTY TESTS - STATE MACHINE
-- ═══════════════════════════════════════════════════════════════════════════════

-- | Property: Initial state is await_greeting
-- Lean: def initState : ConnState := .awaitGreeting
prop_initialStateAwaitGreeting :: Property
prop_initialStateAwaitGreeting =
  zmtpPhase initialZmtpState === PhaseAwaitGreeting
  .&&. BS.null (zmtpBuffer initialZmtpState)

-- ═══════════════════════════════════════════════════════════════════════════════
-- ARBITRARY INSTANCES
-- ═══════════════════════════════════════════════════════════════════════════════

instance Arbitrary Mechanism where
  arbitrary = elements [MechNull, MechPlain, MechCurve]

-- ═══════════════════════════════════════════════════════════════════════════════
-- TEST TREE
-- ═══════════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests = testGroup "ZMTP Properties"
  [ testGroup "Greeting Parsing"
    [ testProperty "valid greeting parses (parseGreeting_deterministic)" prop_validGreetingParses
    , testProperty "greeting exactly 64 bytes (greeting_exactly_64)" prop_greetingExactly64
    , testProperty "incomplete greeting returns Incomplete" prop_incompleteGreeting
    , testProperty "invalid signature triggers ambiguity" prop_invalidSignatureAmbiguous
    , testProperty "old version triggers ambiguity" prop_oldVersionAmbiguous
    , testProperty "unknown mechanism triggers ambiguity" prop_unknownMechanismAmbiguous
    ]
  , testGroup "Frame Header Parsing"
    [ testProperty "reserved flags trigger ambiguity (flags_unambiguous)" prop_reservedFlagsAmbiguous
    , testProperty "short frame header parses" prop_shortFrameHeader
    , testProperty "long frame header needs 9 bytes" prop_longFrameHeaderIncomplete
    , testProperty "long frame header parses" prop_longFrameHeader
    , testProperty "frame too large triggers ambiguity" prop_frameTooLargeAmbiguous
    ]
  , testGroup "State Machine"
    [ testProperty "initial state is await_greeting (reset_is_initial)" prop_initialStateAwaitGreeting
    ]
  ]
