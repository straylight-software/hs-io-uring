-- Comprehensive benchmarks for io-uring library
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Main where

import Control.Monad (replicateM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (bracket)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Text.Printf (printf)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)

import System.IoUring
import System.IoUring.URing (initURing, closeURing, validURing)
import System.IoUring.Reactor
import System.IoUring.ZMQ
import System.IoUring.Logging
import qualified System.ZMQ4 as ZMQ
import Foreign (nullPtr)
import Katip (ls)

-- Benchmark configuration
data BenchConfig = BenchConfig {
    benchIterations :: !Int
  }

defaultConfig :: BenchConfig
defaultConfig = BenchConfig {
    benchIterations = 100000
  }

-- Main entry point
main :: IO ()
main = withLogging "bench" InfoS $ \le -> runKatipContextT le () "main" $ do
  logMsg "bench" InfoS "io-uring Benchmark Suite"
  logMsg "bench" InfoS "=========================="
  
  res1 <- liftIO benchRingCreation
  logMsg "bench" InfoS $ ls res1
  
  res2 <- liftIO benchRingLifecycle
  logMsg "bench" InfoS $ ls res2
  
  res3 <- liftIO benchReactorThroughput
  logMsg "bench" InfoS $ ls res3
  
  res4 <- liftIO benchZmqThroughput
  logMsg "bench" InfoS $ ls res4
  
  logMsg "bench" InfoS "All benchmarks completed!"

-- Helper to measure time
measureTime :: IO a -> IO (a, Double)
measureTime action = do
  start <- getCurrentTime
  result <- action
  end <- getCurrentTime
  let elapsed = realToFrac (diffUTCTime end start) :: Double
  return (result, elapsed)

-- Benchmark: Ring creation
benchRingCreation :: IO String
benchRingCreation = do
  let iterations = 1000
  (_, elapsed) <- measureTime $ 
    replicateM_ iterations $ do
      ring <- initURing 0 256 512
      closeURing ring
  
  let opsPerSec = fromIntegral iterations / elapsed
  return $ printf "Benchmark: Ring Creation\n  %d ring creations in %.3f seconds (%.0f ops/sec)" 
    iterations elapsed opsPerSec

-- Benchmark: Ring lifecycle
benchRingLifecycle :: IO String
benchRingLifecycle = do
  let iterations = 500
  
  (_, elapsed) <- measureTime $ 
    replicateM_ iterations $ do
      bracket 
        (initURing 0 128 256)
        closeURing
        (\ring -> do
          valid <- validURing ring
          void $ return valid)
  
  let opsPerSec = fromIntegral iterations / elapsed
  return $ printf "Benchmark: Ring Lifecycle\n  %d lifecycle iterations in %.3f seconds (%.0f ops/sec)" 
    iterations elapsed opsPerSec

-- Benchmark: Reactor Throughput (NO-OPs)
benchReactorThroughput :: IO String
benchReactorThroughput = do
  let iterations = benchIterations defaultConfig
  
  let params = defaultIoUringParams
  withIoUring params $ \ctx -> do
    withReactor ctx $ \reactor -> do
      (_, elapsed) <- measureTime $ 
        replicateM_ iterations $ do
          -- Using Timeout(0) as NOP
          _ <- submitRequest reactor $ \push ->
              push (TimeoutOp nullPtr 0 0)
          return ()
          
      let opsPerSec = fromIntegral iterations / elapsed
      return $ printf "Benchmark: Reactor Throughput (NO-OP)\n  %d async ops in %.3f seconds (%.0f ops/sec)" 
        iterations elapsed opsPerSec

-- Benchmark: ZMQ Throughput
benchZmqThroughput :: IO String
benchZmqThroughput = do
  let iterations = 50000 -- Smaller count for mixed bench
  
  let params = defaultIoUringParams
  withIoUring params $ \ctx -> do
    withReactor ctx $ \reactor -> do
      ZMQ.withContext $ \zctx -> do
        
        recvAsync <- async $ do
          ZMQ.withSocket zctx ZMQ.Pull $ \sock -> do
            ZMQ.setLinger (ZMQ.restrict (0 :: Int)) sock
            ZMQ.bind sock "inproc://bench_suite"
            
            -- Warmup
            _ <- asyncRecv reactor sock
            
            (_, elapsed) <- measureTime $ 
              replicateM_ iterations $ do
                _ <- asyncRecv reactor sock
                return ()
            return elapsed

        threadDelay 50000
        
        ZMQ.withSocket zctx ZMQ.Push $ \sock -> do
          ZMQ.connect sock "inproc://bench_suite"
          asyncSend reactor sock "warmup"
          replicateM_ iterations $ asyncSend reactor sock "payload"
          
        elapsed <- wait recvAsync
        
        let opsPerSec = fromIntegral iterations / elapsed
        return $ printf "Benchmark: ZMQ Async Throughput\n  %d msg in %.3f seconds (%.0f msg/sec)" 
          iterations elapsed opsPerSec
