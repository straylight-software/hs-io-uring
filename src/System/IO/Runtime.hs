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

-- | The Main Loop.
--
-- This function drives the entire system. It is agnostic to the domain logic.
runReactor
  :: (EventStream s, Reactor r e, Binary e)
  => RuntimeConfig s e
  -> r
  -> IO ()
runReactor config initialR = do
  stateRef <- newIORef initialR

  let
    loop = do
      currentState <- readIORef stateRef

      -- Get next entry based on mode
      mEntry <- case mode config of
        Live -> pollLiveInput config
        Replay -> next (stream config)

      case mEntry of
        Nothing -> return () -- Exit loop or idle
        Just entry -> do
          -- Pure Transition
          let (newState, intents) = react currentState entry

          -- Execute Side Effects
          case mode config of
            Live -> executeIntents intents
            Replay -> verifyIntents intents

          writeIORef stateRef newState
          loop

  loop

-- ════════════════════════════════════════════════════════════════════════════
--                                                                  // helpers
-- ════════════════════════════════════════════════════════════════════════════

pollLiveInput
  :: (EventStream s, Binary e)
  => RuntimeConfig s e
  -> IO (Maybe (Entry e))
pollLiveInput config = do
  mEvent <- tick config
  case mEvent of
    Nothing -> return Nothing
    Just evt -> do
      -- Construct Entry
      -- TODO: Use real timestamp and sequence ID
      let
        entry = Entry
          { sequenceId = 0
          , timestamp = 0
          , checksum = 0
          , event = evt
          }
      -- Persist to Log
      append (stream config) entry
      return (Just entry)

executeIntents :: [OutputIntent] -> IO ()
executeIntents intents = forM_ intents $ \intent -> do
  case intent of
    LogMessage msg -> putStrLn $ "[LIVE] Log: " ++ msg
    SendPacket _ -> putStrLn "[LIVE] Sending Packet..."
    WriteFile path _ -> putStrLn $ "[LIVE] Writing file: " ++ path
    QueryLLM _ -> putStrLn "[LIVE] Querying LLM..."

verifyIntents :: [OutputIntent] -> IO ()
verifyIntents intents = forM_ intents $ \intent -> do
  case intent of
    LogMessage msg -> putStrLn $ "[REPLAY] Log: " ++ msg
    _ -> return ()
