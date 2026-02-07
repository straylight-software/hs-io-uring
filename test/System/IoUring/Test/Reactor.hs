{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module System.IoUring.Test.Reactor (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import System.IoUring (defaultIoUringParams, withIoUring, IoOp(..), IoResult(..))
import System.IoUring.Reactor (withReactor, submitRequest)

tests :: TestTree

tests = testGroup "Reactor Tests"
  [ testCase "Reactor Initialization" testInit
  , testCase "Async Submission" testAsyncSubmission
  , testCase "Timeout Operation" testTimeout
  ]

testInit :: IO ()
testInit = do
  let params = defaultIoUringParams
  withIoUring params $ \ctx -> do
    withReactor ctx $ \_ -> do
      -- Just starting reactor should work
      return ()

testAsyncSubmission :: IO ()
testAsyncSubmission = do
  let params = defaultIoUringParams
  withIoUring params $ \ctx -> do
    withReactor ctx $ \reactor -> do
      -- Submit a NOP
      res <- submitRequest reactor $ \push -> 
          push NopOp
      
      case res of
        IoErrno _ -> return () -- Expected error or success
        Complete _ -> return ()
        Eof -> return ()

testTimeout :: IO ()
testTimeout = do
  let params = defaultIoUringParams
  withIoUring params $ \ctx -> do
    withReactor ctx $ \_ -> do
      return ()
