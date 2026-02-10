{-# LANGUAGE ScopedTypeVariables #-}

module System.IO.Runtime
  ( RuntimeConfig(..)
  , runReactor
  ) where

import System.IO.EventStream 
  ( EventStream(append, next)
  , StreamMode(Live, Replay)
  , Entry(Entry, sequenceId, timestamp, checksum, event)
  )
import System.IO.Reactor (Reactor(react), OutputIntent(LogMessage, SendPacket, WriteFile, QueryLLM))
import Data.Binary (Binary)
import Control.Monad (forM_)
import Data.IORef (newIORef, readIORef, writeIORef)

data RuntimeConfig s e = RuntimeConfig
  { mode    :: StreamMode
  , stream  :: s                 -- ^ The Journal/EventStream
  , tick    :: IO (Maybe e)      -- ^ Source of new events (Live only). Returns Nothing if no event ready.
  }

-- | The Main Loop
runReactor :: (EventStream s, Reactor r e, Binary e) 
           => RuntimeConfig s e 
           -> r 
           -> IO ()
runReactor config initialR = do
  stateRef <- newIORef initialR
  
  let loop = do
        currentState <- readIORef stateRef
        
        -- Get next entry based on mode
        mEntry <- case mode config of
          Live -> do
             -- Poll input source
             mEvent <- tick config
             case mEvent of
               Nothing -> return Nothing -- No new input, maybe sleep?
               Just evt -> do
                 -- Construct Entry (In reality, timestamp would be current time)
                 -- For now we stub sequence/time
                 let entry = Entry 
                       { sequenceId = 0 -- TODO: increment
                       , timestamp  = 0 -- TODO: getMonotonicTime
                       , checksum   = 0 
                       , event      = evt
                       }
                 -- Persist to Log
                 append (stream config) entry
                 return (Just entry)
                 
          Replay -> do
             -- Read from Log
             next (stream config)
        
        case mEntry of
          Nothing -> return () -- Exit loop or idle
          Just entry -> do
             -- Pure Transition
             let (newState, intents) = react currentState entry
             
             -- Execute Side Effects
             case mode config of
               Live -> executeIntents intents
               Replay -> verifyIntents intents -- In replay we might verify against a log of expected outputs
             
             writeIORef stateRef newState
             loop

  loop

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
