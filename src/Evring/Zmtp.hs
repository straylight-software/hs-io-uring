{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | ZMTP 3.x protocol as a Machine instance.
--
-- This module implements the ZeroMQ Message Transport Protocol with
-- the same reset-on-ambiguity discipline as SIGIL. ZMTP 3.x is clean:
--
-- - Deterministic: flags byte fully determines parsing path
-- - Length-prefixed: no escape sequences or delimiters  
-- - Fixed structures: greeting is exactly 64 bytes
--
-- = Wire Format
--
-- @
-- greeting = signature[10] version[2] mechanism[20] as-server[1] filler[31]
-- frame    = flags[1] size[1|8] body[size]
-- @
--
-- = Reset-on-Ambiguity
--
-- - Reserved flags bits non-zero → reset
-- - Invalid greeting signature → reset
-- - Invalid command name → reset
-- - Any protocol violation → reset connection
module Evring.Zmtp
  ( -- * Machine
    ZmtpMachine(..)
    -- * State
  , ZmtpState(..)
  , ConnPhase(..)
    -- * Protocol types
  , Greeting(..)
  , Mechanism(..)
  , FrameHeader(..)
  , Frame(..)
  , ZmtpCommand(..)
    -- * Parse results
  , ZmtpParseResult(..)
  , AmbiguityReason(..)
    -- * Constants
  , greetingSize
  , signatureByte0
  , signatureByte9
    -- * Parsing functions (pure)
  , parseGreeting
  , parseFrameHeader
  , parseFrame
  , parseCommand
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Bits ((.&.), (.|.), testBit, shiftL)
import Data.Word (Word8, Word64)
import GHC.Generics (Generic)

import Evring.Machine
  ( Machine(State, initial, step, done)
  , StepResult(StepResult)
  )
import Evring.Event (Event, Operation)

-- ═══════════════════════════════════════════════════════════════════════════════
-- CONSTANTS
-- ═══════════════════════════════════════════════════════════════════════════════

-- | ZMTP 3.x greeting signature byte 0
signatureByte0 :: Word8
signatureByte0 = 0xFF

-- | ZMTP 3.x greeting signature byte 9
signatureByte9 :: Word8
signatureByte9 = 0x7F

-- | Greeting size in bytes
greetingSize :: Int
greetingSize = 64

-- | Maximum frame size we accept (256 MB)
maxFrameSize :: Word64
maxFrameSize = 256 * 1024 * 1024

-- Flag bits (used via testBit, but keeping constants for documentation)
_flagMore, _flagLong, _flagCommand, flagReservedMask :: Word8
_flagMore    = 0x01  -- Bit 0: more frames follow
_flagLong    = 0x02  -- Bit 1: 8-byte size
_flagCommand = 0x04  -- Bit 2: command frame
flagReservedMask = 0xF8  -- Bits 3-7: reserved

-- ═══════════════════════════════════════════════════════════════════════════════
-- AMBIGUITY REASONS
-- ═══════════════════════════════════════════════════════════════════════════════

-- | Reasons for ambiguity in ZMTP parsing (triggers reset)
data AmbiguityReason
  = InvalidSignature !Word8 !Word8
  | UnsupportedVersion !Word8 !Word8
  | ReservedFlagsSet !Word8
  | InvalidCommandName
  | FrameTooLarge !Word64
  | UnexpectedCommand !ByteString
  | MechanismMismatch !ByteString
  deriving stock (Eq, Show, Generic)

-- ═══════════════════════════════════════════════════════════════════════════════
-- PARSE RESULT
-- ═══════════════════════════════════════════════════════════════════════════════

-- | Strict parse result with three outcomes.
data ZmtpParseResult a
  = Ok !a !ByteString        -- ^ Success with remaining bytes
  | Incomplete !Int          -- ^ Need N more bytes
  | Ambiguous !AmbiguityReason  -- ^ Protocol violation, must reset
  deriving stock (Eq, Show, Generic)

instance Functor ZmtpParseResult where
  fmap f = \case
    Ok a rest -> Ok (f a) rest
    Incomplete n -> Incomplete n
    Ambiguous r -> Ambiguous r

-- ═══════════════════════════════════════════════════════════════════════════════
-- SECURITY MECHANISMS
-- ═══════════════════════════════════════════════════════════════════════════════

-- | ZMTP security mechanisms
data Mechanism
  = MechNull   -- ^ No security
  | MechPlain  -- ^ Username/password (cleartext!)
  | MechCurve  -- ^ CurveZMQ (CurveCP-based)
  deriving stock (Eq, Show, Generic)

-- | Parse mechanism from 20-byte field
parseMechanism :: ByteString -> Maybe Mechanism
parseMechanism bs
  | BS.length bs < 20 = Nothing
  | BS.isPrefixOf "NULL\0" bs = Just MechNull
  | BS.isPrefixOf "PLAIN\0" bs = Just MechPlain
  | BS.isPrefixOf "CURVE\0" bs = Just MechCurve
  | otherwise = Nothing

-- ═══════════════════════════════════════════════════════════════════════════════
-- GREETING
-- ═══════════════════════════════════════════════════════════════════════════════

-- | Parsed ZMTP greeting
data Greeting = Greeting
  { greetingVersionMajor :: !Word8
  , greetingVersionMinor :: !Word8
  , greetingMechanism    :: !Mechanism
  , greetingAsServer     :: !Bool
  } deriving stock (Eq, Show, Generic)

-- | Parse 64-byte ZMTP greeting
parseGreeting :: ByteString -> ZmtpParseResult Greeting
parseGreeting bs
  -- Need exactly 64 bytes
  | BS.length bs < greetingSize = Incomplete (greetingSize - BS.length bs)
  | otherwise =
      let sig0 = BS.index bs 0
          sig9 = BS.index bs 9
      in if sig0 /= signatureByte0 || sig9 /= signatureByte9
         then Ambiguous (InvalidSignature sig0 sig9)
         else
           let major = BS.index bs 10
               minor = BS.index bs 11
           in if major < 3
              then Ambiguous (UnsupportedVersion major minor)
              else
                let mechBytes = BS.take 20 (BS.drop 12 bs)
                in case parseMechanism mechBytes of
                  Nothing -> Ambiguous (MechanismMismatch mechBytes)
                  Just mech ->
                    let asServer = BS.index bs 32 /= 0
                        rest = BS.drop greetingSize bs
                    in Ok (Greeting major minor mech asServer) rest

-- ═══════════════════════════════════════════════════════════════════════════════
-- FRAME HEADER
-- ═══════════════════════════════════════════════════════════════════════════════

-- | Parsed frame header
data FrameHeader = FrameHeader
  { frameSize      :: !Word64
  , frameHasMore   :: !Bool
  , frameIsLong    :: !Bool
  , frameIsCommand :: !Bool
  } deriving stock (Eq, Show, Generic)

-- | Check if flags byte has reserved bits set
hasReservedBits :: Word8 -> Bool
hasReservedBits flags = (flags .&. flagReservedMask) /= 0

-- | Read 8-byte big-endian size
readSize64BE :: ByteString -> Word64
readSize64BE bs = 
  let b0 = fromIntegral (BS.index bs 0) :: Word64
      b1 = fromIntegral (BS.index bs 1) :: Word64
      b2 = fromIntegral (BS.index bs 2) :: Word64
      b3 = fromIntegral (BS.index bs 3) :: Word64
      b4 = fromIntegral (BS.index bs 4) :: Word64
      b5 = fromIntegral (BS.index bs 5) :: Word64
      b6 = fromIntegral (BS.index bs 6) :: Word64
      b7 = fromIntegral (BS.index bs 7) :: Word64
  in (b0 `shiftL` 56) .|. (b1 `shiftL` 48) .|. (b2 `shiftL` 40) .|. (b3 `shiftL` 32)
     .|. (b4 `shiftL` 24) .|. (b5 `shiftL` 16) .|. (b6 `shiftL` 8) .|. b7

-- | Parse frame header (flags + size)
parseFrameHeader :: ByteString -> ZmtpParseResult FrameHeader
parseFrameHeader bs
  | BS.length bs < 1 = Incomplete 1
  | otherwise =
      let flags = BS.index bs 0
      in if hasReservedBits flags
         then Ambiguous (ReservedFlagsSet flags)
         else
           let hasMore   = testBit flags 0
               isLong    = testBit flags 1
               isCommand = testBit flags 2
           in if isLong
              then
                if BS.length bs < 9
                then Incomplete (9 - BS.length bs)
                else
                  let size64 = readSize64BE (BS.drop 1 bs)
                  in if size64 > maxFrameSize
                     then Ambiguous (FrameTooLarge size64)
                     else Ok (FrameHeader size64 hasMore True isCommand) (BS.drop 9 bs)
              else
                if BS.length bs < 2
                then Incomplete (2 - BS.length bs)
                else
                  let size = fromIntegral (BS.index bs 1) :: Word64
                  in Ok (FrameHeader size hasMore False isCommand) (BS.drop 2 bs)

-- ═══════════════════════════════════════════════════════════════════════════════
-- FRAME
-- ═══════════════════════════════════════════════════════════════════════════════

-- | A complete frame
data Frame = Frame
  { frameHeader :: !FrameHeader
  , frameBody   :: !ByteString
  } deriving stock (Eq, Show, Generic)

-- | Parse complete frame (header + body)
parseFrame :: ByteString -> ZmtpParseResult Frame
parseFrame bs = case parseFrameHeader bs of
  Incomplete n -> Incomplete n
  Ambiguous r -> Ambiguous r
  Ok header rest ->
    let size = fromIntegral (frameSize header) :: Int
    in if BS.length rest < size
       then Incomplete (size - BS.length rest)
       else Ok (Frame header (BS.take size rest)) (BS.drop size rest)

-- ═══════════════════════════════════════════════════════════════════════════════
-- COMMANDS
-- ═══════════════════════════════════════════════════════════════════════════════

-- | Known ZMTP commands
data ZmtpCommand
  = CmdReady [(ByteString, ByteString)]  -- ^ READY with properties
  | CmdError !ByteString                  -- ^ ERROR with reason
  | CmdSubscribe !ByteString              -- ^ SUBSCRIBE (SUB socket)
  | CmdCancel !ByteString                 -- ^ CANCEL (SUB socket)
  | CmdPing !ByteString                   -- ^ PING
  | CmdPong !ByteString                   -- ^ PONG
  | CmdUnknown !ByteString !ByteString    -- ^ Unknown command
  deriving stock (Eq, Show, Generic)

-- | Check if byte is printable ASCII
isPrintableAscii :: Word8 -> Bool
isPrintableAscii b = b >= 0x20 && b <= 0x7E

-- | Parse command from frame body
parseCommand :: ByteString -> ZmtpParseResult ZmtpCommand
parseCommand body
  | BS.length body < 1 = Ambiguous InvalidCommandName
  | otherwise =
      let nameLen = fromIntegral (BS.index body 0) :: Int
      in if nameLen == 0 || nameLen > 255
         then Ambiguous InvalidCommandName
         else if BS.length body < 1 + nameLen
              then Incomplete (1 + nameLen - BS.length body)
              else
                let nameBytes = BS.take nameLen (BS.drop 1 body)
                in if not (BS.all isPrintableAscii nameBytes)
                   then Ambiguous InvalidCommandName
                   else
                     let cmdData = BS.drop (1 + nameLen) body
                         cmd = case nameBytes of
                           "READY"     -> CmdReady []  -- TODO: parse properties
                           "ERROR"     -> CmdError cmdData
                           "SUBSCRIBE" -> CmdSubscribe cmdData
                           "CANCEL"    -> CmdCancel cmdData
                           "PING"      -> CmdPing cmdData
                           "PONG"      -> CmdPong cmdData
                           _           -> CmdUnknown nameBytes cmdData
                     in Ok cmd BS.empty

-- ═══════════════════════════════════════════════════════════════════════════════
-- MACHINE STATE
-- ═══════════════════════════════════════════════════════════════════════════════

-- | Connection phase
data ConnPhase
  = PhaseAwaitGreeting
  | PhaseAwaitHandshake !Greeting
  | PhaseReady !Greeting
  | PhaseFailed !AmbiguityReason
  deriving stock (Eq, Show, Generic)

-- | ZMTP connection state
data ZmtpState = ZmtpState
  { zmtpPhase  :: !ConnPhase
  , zmtpBuffer :: !ByteString  -- ^ Accumulated bytes
  } deriving stock (Eq, Show, Generic)

-- | Initial state
initialZmtpState :: ZmtpState
initialZmtpState = ZmtpState PhaseAwaitGreeting BS.empty

-- | Reset state (on ambiguity)
_resetZmtpState :: ZmtpState
_resetZmtpState = initialZmtpState

-- ═══════════════════════════════════════════════════════════════════════════════
-- MACHINE INSTANCE
-- ═══════════════════════════════════════════════════════════════════════════════

-- | ZMTP protocol machine
data ZmtpMachine = ZmtpMachine
  { zmtpAsServer :: !Bool  -- ^ Are we the server?
  } deriving stock (Eq, Show, Generic)

instance Machine ZmtpMachine where
  type State ZmtpMachine = ZmtpState

  initial _ = initialZmtpState

  step machine state event =
    -- Append any received data to buffer
    let newBuf = zmtpBuffer state <> eventDataBytes event
        state' = state { zmtpBuffer = newBuf }
    in case zmtpPhase state' of
      PhaseAwaitGreeting ->
        case parseGreeting newBuf of
          Incomplete _ -> StepResult state' []
          Ambiguous reason -> StepResult (state' { zmtpPhase = PhaseFailed reason }) []
          Ok greeting rest ->
            -- Send our greeting, move to handshake
            let state'' = state' { zmtpPhase = PhaseAwaitHandshake greeting
                                 , zmtpBuffer = rest }
            in StepResult state'' [makeGreetingOp machine]
      
      PhaseAwaitHandshake greeting ->
        -- Await READY command
        case parseFrame newBuf of
          Incomplete _ -> StepResult state' []
          Ambiguous reason -> StepResult (state' { zmtpPhase = PhaseFailed reason }) []
          Ok frame rest ->
            if frameIsCommand (frameHeader frame)
            then case parseCommand (frameBody frame) of
              Ambiguous reason -> StepResult (state' { zmtpPhase = PhaseFailed reason }) []
              Incomplete _ -> StepResult state' []  -- Shouldn't happen for commands
              Ok (CmdReady _) _ ->
                let state'' = state' { zmtpPhase = PhaseReady greeting
                                     , zmtpBuffer = rest }
                in StepResult state'' [makeReadyOp]
              Ok cmd _ ->
                let reason = UnexpectedCommand (cmdName cmd)
                in StepResult (state' { zmtpPhase = PhaseFailed reason }) []
            else
              -- Message frame during handshake = error
              let reason = UnexpectedCommand "message-during-handshake"
              in StepResult (state' { zmtpPhase = PhaseFailed reason }) []
      
      PhaseReady _greeting ->
        -- Ready to process messages
        case parseFrame newBuf of
          Incomplete _ -> StepResult state' []
          Ambiguous reason -> StepResult (state' { zmtpPhase = PhaseFailed reason }) []
          Ok _frame rest ->
            let state'' = state' { zmtpBuffer = rest }
            in StepResult state'' []  -- TODO: emit message events
      
      PhaseFailed _ ->
        -- Stay in failed state
        StepResult state' []

  done _ state = case zmtpPhase state of
    PhaseFailed _ -> True
    _ -> False

-- ═══════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ═══════════════════════════════════════════════════════════════════════════════

-- | Extract bytes from event (placeholder - real impl would use Event.eventData)
eventDataBytes :: Event -> ByteString
eventDataBytes _ = BS.empty  -- TODO: extract from Event

-- | Get command name for error reporting
cmdName :: ZmtpCommand -> ByteString
cmdName = \case
  CmdReady _     -> "READY"
  CmdError _     -> "ERROR"
  CmdSubscribe _ -> "SUBSCRIBE"
  CmdCancel _    -> "CANCEL"
  CmdPing _      -> "PING"
  CmdPong _      -> "PONG"
  CmdUnknown n _ -> n

-- | Create greeting operation (placeholder)
makeGreetingOp :: ZmtpMachine -> Operation
makeGreetingOp _ = error "TODO: implement greeting operation"

-- | Create READY command operation (placeholder)
makeReadyOp :: Operation
makeReadyOp = error "TODO: implement ready operation"
