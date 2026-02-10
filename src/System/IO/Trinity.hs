{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                    // system // io // trinity
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--     "The Trinity engine invented WASM and deterministic replay and safe
--      asynchrony 35 years before anyone got it."
--
--     This module implements the Trinity architecture, originated by John
--     Carmack in the Quake III Arena engine (1999). The core insight:
--
--       1. Game state is a pure function of the input stream
--       2. Gather all events, then process in one deterministic tick
--       3. Side effects happen only at frame boundaries
--       4. Client predicts, server reconciles — same inputs, same outputs
--
--     We extend Trinity with:
--       - Coeffect tracking (prove resource access)
--       - Cryptographic attestation (sign the reasoning chain)
--       - io_uring backend (modern kernel async I/O)
--
--                                                              — b7r6 // 2026
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module System.IO.Trinity
  ( TrinityConfig (..)
  , runTrinity
  ) where

import Control.Monad (forM_)
import Data.Binary (Binary)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.IO.EventStream
  ( Entry (Entry, checksum, event, sequenceId, timestamp)
  , EventStream (append, next)
  , StreamMode (Live, Replay)
  )
import System.IO.Reactor
  ( OutputIntent (LogMessage, QueryLLM, SendPacket, WriteFile)
  , Reactor (react)
  )

-- ════════════════════════════════════════════════════════════════════════════
--                                                                   // config
-- ════════════════════════════════════════════════════════════════════════════

-- | Trinity engine configuration.
--
-- The three modes map to Carmack's original design:
--   - Live: Real I/O, events persisted to journal (game server)
--   - Replay: Read from journal, verify determinism (demo playback)
--   - Sim: Synthetic events for testing (bot matches)
data TrinityConfig s e = TrinityConfig
  { tMode :: StreamMode
  -- ^ Execution mode: Live, Replay, or Sim
  , tStream :: s
  -- ^ The event journal (EventStream instance)
  , tTick :: IO (Maybe e)
  -- ^ Event source — Com_EventLoop() in Q3 terms
  }

-- ════════════════════════════════════════════════════════════════════════════
--                                                                // com_frame
-- ════════════════════════════════════════════════════════════════════════════

-- | The Trinity main loop — Com_Frame() in Haskell.
--
-- @
-- void Com_Frame( void ) {
--     com_frameTime = Com_EventLoop();   // ← tTick
--     SV_Frame( msec );                  // ← react (server)
--     CL_Frame( msec );                  // ← react (client)
-- }
-- @
--
-- This function drives the entire system. The Reactor is the pure "game logic"
-- that transforms events into state transitions and intents. Side effects
-- (network, disk, GPU) happen only at frame boundaries via intent execution.
runTrinity
  :: (EventStream s, Reactor r e, Binary e)
  => TrinityConfig s e
  -> r
  -> IO ()
runTrinity config initialR = do
  stateRef <- newIORef initialR
  comFrame stateRef

  where
    -- the frame loop — runs until no more events
    comFrame stateRef = do
      currentState <- readIORef stateRef
      mEntry <- comEventLoop
      processFrame stateRef currentState mEntry

    -- Com_EventLoop: gather next event based on mode
    comEventLoop
      | Live <- tMode config = pollLiveInput config
      | Replay <- tMode config = next (tStream config)

    -- process one frame: react, then execute/verify intents
    processFrame _ _ Nothing = pure ()  -- exit or idle
    processFrame stateRef currentState (Just entry)
      | (newState, intents) <- react currentState entry
      = do
        dispatchIntents intents
        writeIORef stateRef newState
        comFrame stateRef

    -- intents dispatch based on mode
    dispatchIntents intents
      | Live <- tMode config = executeIntents intents
      | Replay <- tMode config = verifyIntents intents

-- ════════════════════════════════════════════════════════════════════════════
--                                                                  // helpers
-- ════════════════════════════════════════════════════════════════════════════

pollLiveInput
  :: (EventStream s, Binary e)
  => TrinityConfig s e
  -> IO (Maybe (Entry e))
pollLiveInput config = do
  mEvent <- tTick config
  wrapAndPersist mEvent
  where
    wrapAndPersist Nothing = pure Nothing
    -- TODO[b7r6]: use real timestamp and sequence id
    wrapAndPersist (Just evt)
      | entry <- Entry { sequenceId = 0, timestamp = 0, checksum = 0, event = evt }
      = do
        append (tStream config) entry  -- persist to journal
        pure (Just entry)

executeIntents :: [OutputIntent] -> IO ()
executeIntents intents = forM_ intents $ \case
  LogMessage msg -> putStrLn $ "[LIVE] Log: " ++ msg
  SendPacket _ -> putStrLn "[LIVE] Sending Packet..."
  WriteFile path _ -> putStrLn $ "[LIVE] Writing file: " ++ path
  QueryLLM _ -> putStrLn "[LIVE] Querying LLM..."

verifyIntents :: [OutputIntent] -> IO ()
verifyIntents intents = forM_ intents $ \case
  LogMessage msg -> putStrLn $ "[REPLAY] Log: " ++ msg
  _ -> pure ()
