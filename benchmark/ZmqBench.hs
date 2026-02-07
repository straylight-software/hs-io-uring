{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.ZMQ4 (Context, withContext, withSocket, bind, connect, setLinger)
import qualified System.ZMQ4 as ZMQ

import System.IoUring (defaultIoUringParams, withIoUring, ioBatchSizeLimit, ioConcurrencyLimit)
import System.IoUring.Reactor (Reactor, withReactor)
import System.IoUring.ZMQ (asyncRecv, asyncSend)
import System.IoUring.Logging (runKatipContextT, logMsg, Severity(InfoS), KatipContextT)
import System.IoUring.Options (withOptions)
import Katip (ls, getLogEnv, getKatipNamespace, getKatipContext)

msgCount :: Int
msgCount = 100000

msgSize :: Int
msgSize = 100

message :: ByteString
message = BSC.pack $ replicate msgSize 'a'

main :: IO ()
main = withOptions "zmq-bench" $ \_opts le -> runKatipContextT le () "main" $ do
  logMsg "bench" InfoS $ ls ("Starting ZMQ Benchmark with " ++ show msgCount ++ " messages of size " ++ show msgSize)

  
  -- Initialize io_uring
  let params = defaultIoUringParams { ioBatchSizeLimit = 128, ioConcurrencyLimit = 256 }
  liftIO $ withIoUring params $ \ctx -> do
    withReactor ctx $ \reactor -> do
      withContext $ \zmqCtx -> do
        
        -- Start Receiver
        r <- async $ runKatipContextT le () "receiver" $ runReceiver reactor zmqCtx
        
        -- Give receiver time to bind
        threadDelay 100000
        
        -- Start Sender
        s <- async $ runKatipContextT le () "sender" $ runSender reactor zmqCtx
        
        wait s
        wait r

runReceiver :: Reactor -> Context -> KatipContextT IO ()
runReceiver reactor ctx = do
  liftIO $ withSocket ctx ZMQ.Pull $ \sock -> do
    setLinger (ZMQ.restrict (0 :: Int)) sock -- Don't linger on close
    bind sock "inproc://bench"
    
    -- We can't log here easily without lifting again.
    -- But we can return values or use runKatipContextT inside IO if we really wanted.
    -- Since this is a benchmark callback, let's keep it clean and log outside or pass LogEnv.
    -- But `withSocket` runs in IO.
    -- I'll just skip detailed logging inside the tight loop/callback setup for simplicity,
    -- or assume `runReceiver` structure handles it.
    
    -- Wait, `runReceiver` runs in `KatipContextT`.
    -- `withSocket` takes callback `Socket -> IO a`.
    -- So inside callback is IO.
    -- I can use `unlift` pattern or `runKatipContextT` if I capture `le`.
    -- Or just liftIO the logic.
    return ()
    
  -- REFACTOR to liftIO around withSocket? No, withSocket expects IO.
  
  -- I'll use `le` capture from closure?
  -- runReceiver :: Reactor -> Context -> KatipContextT IO ()
  -- I need `le` inside. `getLogEnv`?
  le <- getLogEnv
  ns <- getKatipNamespace
  ctx' <- getKatipContext
  
  liftIO $ withSocket ctx ZMQ.Pull $ \sock -> do
    setLinger (ZMQ.restrict (0 :: Int)) sock
    bind sock "inproc://bench"
    runKatipContextT le ctx' ns $ logMsg "bench" InfoS "Receiver bound."
    
    -- Warmup? No, simplistic bench.
    
    start <- getCurrentTime
    
    replicateM_ msgCount $ do
      _ <- asyncRecv reactor sock
      return ()
      
    end <- getCurrentTime
    let diff = diffUTCTime end start
    let seconds = realToFrac diff :: Double
    let msgsPerSec = fromIntegral msgCount / seconds
    
    runKatipContextT le ctx' ns $ logMsg "bench" InfoS $ ls ("Receiver finished in " ++ show diff)
    runKatipContextT le ctx' ns $ logMsg "bench" InfoS $ ls ("Throughput: " ++ show (round msgsPerSec :: Int) ++ " msgs/sec")

runSender :: Reactor -> Context -> KatipContextT IO ()
runSender reactor ctx = do
  le <- getLogEnv
  ns <- getKatipNamespace
  ctx' <- getKatipContext
  
  liftIO $ withSocket ctx ZMQ.Push $ \sock -> do
    connect sock "inproc://bench"
    runKatipContextT le ctx' ns $ logMsg "bench" InfoS "Sender connected."
    
    replicateM_ msgCount $ do
      asyncSend reactor sock message
      
    runKatipContextT le ctx' ns $ logMsg "bench" InfoS "Sender finished."
