{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module System.IoUring.Test.Stress (tests) where

import Control.Monad (replicateM_, forM_)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=), assertBool)

import System.IoUring
import System.IoUring.Reactor

tests :: TestTree
tests = testGroup "Stress Tests"
  [ testCase "High Throughput (100k NOPs)" testHighThroughput
  , testCase "Batch Submission" testBatchSubmission
  ]

testHighThroughput :: Assertion
testHighThroughput = do
  let params = defaultIoUringParams { ioBatchSizeLimit = 128, ioConcurrencyLimit = 256 }
  withIoUring params $ \ctx -> do
    withReactor ctx $ \reactor -> do
      let nOps = 100000
      replicateM_ nOps $ do
        res <- submitRequest reactor $ \push ->
            push NopOp
        case res of
          Complete _ -> return ()
          _ -> assertBool "Op failed" False

testBatchSubmission :: Assertion
testBatchSubmission = do
  -- Test submitBatch directly
  let params = defaultIoUringParams
  withIoUring params $ \ctx -> do
    let batchSize = 100
    results <- submitBatch ctx $ \push ->
        replicateM_ batchSize $ push NopOp
    
    length results @?= batchSize
    forM_ results $ \r -> case r of
        Complete _ -> return ()
        _ -> assertBool "Op failed" False
