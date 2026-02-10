{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                   // system // io // runtime
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module System.IO.Runtime
  ( RuntimeConfig (..)
  , runReactor
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
--                                                               // config
-- ════════════════════════════════════════════════════════════════════════════

data RuntimeConfig s e = RuntimeConfig
  { mode :: StreamMode
  , stream :: s
  -- ^ The Journal/EventStream.
  , tick :: IO (Maybe e)
  -- ^ Source of new events (Live only). Returns Nothing if no event ready.
  }

-- ════════════════════════════════════════════════════════════════════════════
--                                                               // main loop
-- ════════════════════════════════════════════════════════════════════════════

-- | The main loop.
--
-- This function drives the entire system. It is agnostic to the domain logic.
runReactor
  :: (EventStream s, Reactor r e, Binary e)
  => RuntimeConfig s e
  -> r
  -> IO ()
runReactor config initialR = do
  stateRef <- newIORef initialR
  loop stateRef

  where
    loop stateRef = do
      currentState <- readIORef stateRef
      mEntry <- getNextEntry
      processEntry stateRef currentState mEntry

    getNextEntry
      | Live <- mode config = pollLiveInput config
      | Replay <- mode config = next (stream config)

    processEntry _ _ Nothing = pure ()  -- exit loop or idle
    processEntry stateRef currentState (Just entry)
      | (newState, intents) <- react currentState entry
      = do
        executeOrVerify intents
        writeIORef stateRef newState
        loop stateRef

    executeOrVerify intents
      | Live <- mode config = executeIntents intents
      | Replay <- mode config = verifyIntents intents

-- ════════════════════════════════════════════════════════════════════════════
--                                                                  // helpers
-- ════════════════════════════════════════════════════════════════════════════

pollLiveInput
  :: (EventStream s, Binary e)
  => RuntimeConfig s e
  -> IO (Maybe (Entry e))
pollLiveInput config = do
  mEvent <- tick config
  wrapAndPersist mEvent
  where
    wrapAndPersist Nothing = pure Nothing
    -- TODO[b7r6]: use real timestamp and sequence id
    wrapAndPersist (Just evt)
      | entry <- Entry { sequenceId = 0, timestamp = 0, checksum = 0, event = evt }
      = do
        append (stream config) entry  -- persist to log
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
