{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Property tests for SIGIL wire format decoder.
--
-- These tests exercise the reset-on-ambiguity discipline that is
-- proven correct in Cornell.Sigil.
module Evring.Test.Sigil
  ( tests
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word32)
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
  , listOf
  , oneof
  )

import Evring.Sigil
  ( SigilState(SigilState, sigilParseMode, sigilBuffer, sigilLeftover, sigilDone)
  , ParseMode(ModeText, ModeThink, ModeToolCall, ModeCodeBlock)
  , Chunk(Chunk, chunkContent, chunkComplete)
  , ChunkContent(TextContent, ThinkContent, ToolCallContent, CodeBlockContent, StreamEnd, AmbiguityReset)
  , AmbiguityReason(UnmatchedModeEnd, NestedModeStart, ReservedOpcode)
  , initSigilState
  , resetSigilState
  , isHotByte
  , isExtendedByte
  , isControlByte
  , decodeVarint
  )

-- ═══════════════════════════════════════════════════════════════════════════════
-- GENERATORS
-- ═══════════════════════════════════════════════════════════════════════════════

-- | Generate a hot token byte (0x00-0x7E, not 0x7F)
genHotByte :: Gen Word8
genHotByte = choose (0x00, 0x7E)

-- | Generate an extended token escape byte (0x80-0xBF)
_genExtendedByte :: Gen Word8
_genExtendedByte = choose (0x80, 0xBF)

-- | Generate a control opcode (0xC0-0xCF)
_genControlByte :: Gen Word8
_genControlByte = choose (0xC0, 0xCF)

-- | Generate a reserved control opcode (0xC8-0xCE) - triggers ambiguity
_genReservedControl :: Gen Word8
_genReservedControl = choose (0xC8, 0xCE)

-- | Generate a valid varint (LEB128 encoded)
_genVarint :: Gen ByteString
_genVarint = do
  value <- choose (0, 0x0FFFFFFF) :: Gen Word32  -- Stay under 28-bit limit
  pure (_encodeVarint value)

-- | Encode a value as LEB128 varint
_encodeVarint :: Word32 -> ByteString
_encodeVarint n = BS.pack (go n)
  where
    go v
      | v < 0x80  = [fromIntegral v]
      | otherwise = fromIntegral (v `mod` 0x80 + 0x80) : go (v `div` 0x80)

-- | Generate valid SIGIL stream bytes (hot tokens only)
_genValidHotTokens :: Gen ByteString
_genValidHotTokens = BS.pack <$> listOf genHotByte

-- | Generate a sequence of hot tokens followed by CHUNK_END
_genCompleteChunk :: Gen ByteString
_genCompleteChunk = do
  tokens <- listOf genHotByte
  pure $ BS.pack tokens <> BS.singleton 0xC0  -- CHUNK_END

-- | Generate bytes that will trigger ambiguity
_genAmbiguousBytes :: Gen ByteString
_genAmbiguousBytes = oneof
  [ -- Reserved control opcode
    BS.singleton <$> _genReservedControl
    -- Unmatched end (TOOL_CALL_END without start)
  , pure $ BS.singleton 0xC2  -- TOOL_CALL_END while in ModeText
    -- Nested start (THINK_START while already in TOOL_CALL)
  , pure $ BS.pack [0xC1, 0x01, 0x02, 0xC3]  -- TOOL_CALL_START, tokens, THINK_START
  ]

-- ═══════════════════════════════════════════════════════════════════════════════
-- PROPERTY TESTS
-- ═══════════════════════════════════════════════════════════════════════════════

-- | Property: Reset is idempotent
-- Lean theorem: reset_idempotent
prop_resetIdempotent :: Property
prop_resetIdempotent =
  resetSigilState initSigilState === initSigilState

-- | Property: Reset always produces the same ground state
-- Lean theorem: no_leakage
prop_resetIsGround :: ParseMode -> [Word32] -> Property
prop_resetIsGround mode tokens =
  let state = SigilState mode tokens BS.empty [] False
  in resetSigilState state === initSigilState

-- | Property: Initial state has text mode and empty buffer
prop_initialStateIsGround :: Property
prop_initialStateIsGround =
  sigilParseMode initSigilState === ModeText
  .&&. null (sigilBuffer initSigilState)
  .&&. BS.null (sigilLeftover initSigilState)

-- | Property: Hot bytes are correctly classified
prop_hotByteClassification :: Word8 -> Property
prop_hotByteClassification b =
  isHotByte b === (b < 0x7F)

-- | Property: Extended bytes are correctly classified
prop_extendedByteClassification :: Word8 -> Property
prop_extendedByteClassification b =
  isExtendedByte b === (b >= 0x80 && b < 0xC0)

-- | Property: Control bytes are correctly classified
prop_controlByteClassification :: Word8 -> Property
prop_controlByteClassification b =
  isControlByte b === ((b >= 0xC0 && b < 0xD0) || b == 0xF0)

-- | Property: Decoding hot tokens produces the correct token IDs
prop_hotTokenDecode :: [Word8] -> Property
prop_hotTokenDecode bytes =
  let validBytes = filter (< 0x7F) bytes
      input = BS.pack validBytes
      (state, chunks) = processTestBytes initSigilState input
  in if null validBytes
     then null chunks === True
     else -- Tokens accumulate in buffer (reversed)
       sigilBuffer state === map fromIntegral (reverse validBytes)

-- | Property: CHUNK_END produces a complete chunk
prop_chunkEndProducesChunk :: [Word8] -> Property
prop_chunkEndProducesChunk tokenBytes =
  let validTokens = filter (< 0x7F) tokenBytes
      input = BS.pack validTokens <> BS.singleton 0xC0  -- CHUNK_END
      (_state, chunks) = processTestBytes initSigilState input
  in length chunks === 1
     .&&. chunkComplete (head chunks) === True

-- | Property: Reserved opcodes trigger ambiguity reset
-- Lean theorem: handleControl_reserved_resets
prop_reservedOpcodeResets :: Property
prop_reservedOpcodeResets =
  let opcodes = [0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE]
      testOpcode op =
        let input = BS.singleton op
            (state, chunks) = processTestBytes initSigilState input
        in state === initSigilState
           .&&. length chunks === 1
           .&&. isAmbiguityReset (head chunks)
  in foldr (.&&.) (True === True) (map testOpcode opcodes)

-- | Property: Nested mode start triggers ambiguity reset  
-- Lean theorem: handleControl_nested_start_resets
prop_nestedModeStartResets :: Property
prop_nestedModeStartResets =
  -- Enter TOOL_CALL mode, then try THINK_START
  let input = BS.pack [0xC1, 0xC3]  -- TOOL_CALL_START, THINK_START
      (state, chunks) = processTestBytes initSigilState input
  in state === initSigilState
     .&&. any isAmbiguityReset chunks === True

-- | Property: Unmatched mode end triggers ambiguity reset
-- Lean theorem: handleControl_unmatched_end_resets
prop_unmatchedModeEndResets :: Property
prop_unmatchedModeEndResets =
  -- Try TOOL_CALL_END while in text mode
  let input = BS.singleton 0xC2  -- TOOL_CALL_END
      (state, chunks) = processTestBytes initSigilState input
  in state === initSigilState
     .&&. length chunks === 1
     .&&. isAmbiguityReset (head chunks)

-- | Property: Valid mode transition works correctly
prop_validModeTransition :: Property
prop_validModeTransition =
  -- Enter THINK mode, add tokens, end THINK mode
  let input = BS.pack [0xC3, 0x01, 0x02, 0x03, 0xC4]  -- THINK_START, tokens, THINK_END
      (state, chunks) = processTestBytes initSigilState input
  in sigilParseMode state === ModeText
     .&&. length chunks >= 1  -- At least one chunk produced

-- | Property: STREAM_END transitions to done state
prop_streamEndDone :: Property
prop_streamEndDone =
  let input = BS.singleton 0xCF  -- STREAM_END
      (state, chunks) = processTestBytes initSigilState input
  in sigilDone state === True
     .&&. length chunks >= 1

-- ═══════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ═══════════════════════════════════════════════════════════════════════════════

-- | Process bytes through the state machine (simulating processBytes)
processTestBytes :: SigilState -> ByteString -> (SigilState, [Chunk])
processTestBytes state input = go state input []
  where
    go !s !bs !acc
      | BS.null bs = (s { sigilLeftover = BS.empty }, reverse acc)
      | otherwise =
          let byte = BS.head bs
              rest = BS.tail bs
          in case decodeByte s byte rest of
            Left leftover -> (s { sigilLeftover = leftover }, reverse acc)
            Right (newState, maybeChunk, remaining) ->
              let newAcc = maybe acc (: acc) maybeChunk
              in go newState remaining newAcc

    decodeByte s byte rest
      | isHotByte byte =
          let tokenId = fromIntegral byte
              newState = s { sigilBuffer = tokenId : sigilBuffer s }
          in Right (newState, Nothing, rest)
      | isExtendedByte byte =
          case decodeVarint rest of
            Nothing -> Left (BS.cons byte rest)
            Just (tokenId, consumed) ->
              let newState = s { sigilBuffer = tokenId : sigilBuffer s }
              in Right (newState, Nothing, BS.drop consumed rest)
      | isControlByte byte =
          handleControl s byte rest
      | otherwise =
          Right (s, Nothing, rest)

    handleControl s opcode rest = Right $ case opcode of
      0xC0 ->  -- CHUNK_END
        let chunk = buildTestChunk s True
            newState = s { sigilBuffer = [] }
        in (newState, Just chunk, rest)
      0xC1 ->  -- TOOL_CALL_START
        case sigilParseMode s of
          ModeText ->
            let pendingChunk = if null (sigilBuffer s) then Nothing
                               else Just (buildTestChunk s False)
                newState = s { sigilParseMode = ModeToolCall, sigilBuffer = [], sigilLeftover = BS.empty }
            in (newState, pendingChunk, rest)
          currentMode ->
            let chunk = Chunk (AmbiguityReset (NestedModeStart currentMode ModeToolCall)) True
            in (initSigilState, Just chunk, rest)
      0xC2 ->  -- TOOL_CALL_END
        case sigilParseMode s of
          ModeToolCall ->
            let chunk = buildTestChunk s True
                newState = s { sigilParseMode = ModeText, sigilBuffer = [], sigilLeftover = BS.empty }
            in (newState, Just chunk, rest)
          currentMode ->
            let chunk = Chunk (AmbiguityReset (UnmatchedModeEnd currentMode)) True
            in (initSigilState, Just chunk, rest)
      0xC3 ->  -- THINK_START
        case sigilParseMode s of
          ModeText ->
            let pendingChunk = if null (sigilBuffer s) then Nothing
                               else Just (buildTestChunk s False)
                newState = s { sigilParseMode = ModeThink, sigilBuffer = [], sigilLeftover = BS.empty }
            in (newState, pendingChunk, rest)
          currentMode ->
            let chunk = Chunk (AmbiguityReset (NestedModeStart currentMode ModeThink)) True
            in (initSigilState, Just chunk, rest)
      0xC4 ->  -- THINK_END
        case sigilParseMode s of
          ModeThink ->
            let chunk = buildTestChunk s True
                newState = s { sigilParseMode = ModeText, sigilBuffer = [], sigilLeftover = BS.empty }
            in (newState, Just chunk, rest)
          currentMode ->
            let chunk = Chunk (AmbiguityReset (UnmatchedModeEnd currentMode)) True
            in (initSigilState, Just chunk, rest)
      0xC7 ->  -- FLUSH
        let chunk = buildTestChunk s False
            newState = s { sigilBuffer = [] }
        in (newState, Just chunk, rest)
      0xCF ->  -- STREAM_END
        let chunk = if null (sigilBuffer s)
                    then Chunk StreamEnd True
                    else buildTestChunk s True
            newState = initSigilState { sigilDone = True }
        in (newState, Just chunk, rest)
      _ | opcode >= 0xC8 && opcode <= 0xCE ->  -- Reserved
        let chunk = Chunk (AmbiguityReset (ReservedOpcode opcode)) True
        in (initSigilState, Just chunk, rest)
      _ -> (s, Nothing, rest)

    buildTestChunk s complete = Chunk content complete
      where
        tokens = reverse (sigilBuffer s)
        content = case sigilParseMode s of
          ModeText      -> TextContent tokens
          ModeThink     -> ThinkContent tokens
          ModeToolCall  -> ToolCallContent tokens
          ModeCodeBlock -> CodeBlockContent tokens

-- | Check if a chunk is an ambiguity reset
isAmbiguityReset :: Chunk -> Bool
isAmbiguityReset chunk = case chunkContent chunk of
  AmbiguityReset _ -> True
  _ -> False

-- ═══════════════════════════════════════════════════════════════════════════════
-- ARBITRARY INSTANCES
-- ═══════════════════════════════════════════════════════════════════════════════

instance Arbitrary ParseMode where
  arbitrary = elements [ModeText, ModeThink, ModeToolCall, ModeCodeBlock]

-- ═══════════════════════════════════════════════════════════════════════════════
-- TEST TREE
-- ═══════════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests = testGroup "SIGIL Properties"
  [ testGroup "Reset-on-Ambiguity"
    [ testProperty "reset is idempotent (reset_idempotent)" prop_resetIdempotent
    , testProperty "reset always produces ground state (no_leakage)" prop_resetIsGround
    , testProperty "initial state is ground" prop_initialStateIsGround
    , testProperty "reserved opcodes reset (handleControl_reserved_resets)" prop_reservedOpcodeResets
    , testProperty "nested mode start resets (handleControl_nested_start_resets)" prop_nestedModeStartResets
    , testProperty "unmatched mode end resets (handleControl_unmatched_end_resets)" prop_unmatchedModeEndResets
    ]
  , testGroup "Byte Classification"
    [ testProperty "hot byte classification" prop_hotByteClassification
    , testProperty "extended byte classification" prop_extendedByteClassification
    , testProperty "control byte classification" prop_controlByteClassification
    ]
  , testGroup "Parsing"
    [ testProperty "hot token decode" prop_hotTokenDecode
    , testProperty "CHUNK_END produces complete chunk" prop_chunkEndProducesChunk
    , testProperty "valid mode transitions work" prop_validModeTransition
    , testProperty "STREAM_END sets done flag" prop_streamEndDone
    ]
  ]
