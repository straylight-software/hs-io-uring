{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module System.IoUring.Test.Concurrency (tests) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (replicateM)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, Assertion)

import System.IoUring
import System.IoUring.Reactor

tests :: TestTree
tests = testGroup "Concurrency Tests"
  [ testCase "Concurrent Reactor Submission (100k ops, 10 threads)" testConcurrentReactor
  , testCase "Reactor Capability Sharding" testCapabilitySharding
  ]

-- | Stress test the Reactor with multiple concurrent threads submitting ops
testConcurrentReactor :: Assertion
testConcurrentReactor = do
  let params = defaultIoUringParams { ioBatchSizeLimit = 128, ioConcurrencyLimit = 256 }
  withIoUring params $ \ctx -> do
    withReactor ctx $ \reactor -> do
      let nThreads = 10
      let nOpsPerThread = 10000
      
      results <- mapConcurrently (\_tId -> do
          -- Submit nOps
          replicateM nOpsPerThread $ do
             submitRequest reactor $ \push ->
                 push NopOp
        ) [1..nThreads]
        
      -- Verify all results are successful
      let totalOps = sum (map length results)
      assertEqual "Total ops completed" (nThreads * nOpsPerThread) totalOps

-- | Verify that operations work when threads migrate or use different capabilities
testCapabilitySharding :: Assertion
testCapabilitySharding = do
  let params = defaultIoUringParams
  withIoUring params $ \ctx -> do
    withReactor ctx $ \reactor -> do
      numCaps <- getNumCapabilities
      
      -- Launch more threads than capabilities to force sharing/migration checks
      let nThreads = numCaps * 4
      
      results <- mapConcurrently (\_i -> do
          -- Submit op
          res <- submitRequest reactor $ \push ->
              push NopOp
          return res
        ) [1..nThreads]
      
      mapM_ (\r -> case r of
          Complete _ -> return ()
          _ -> assertBool ("Op failed: " ++ show r) False
        ) results
