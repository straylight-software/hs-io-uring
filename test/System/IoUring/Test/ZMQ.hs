{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module System.IoUring.Test.ZMQ (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)
import System.IoUring
import System.IoUring.Reactor
import System.IoUring.ZMQ
import System.ZMQ4 (withContext, withSocket, bind, connect, setLinger, restrict)
import qualified System.ZMQ4 as ZMQ
import Control.Concurrent.Async (async, wait)
import Control.Concurrent (threadDelay)

tests :: TestTree
tests = testGroup "ZMQ Integration Tests"
  [ testCase "Async Push/Pull" testPushPull
  ]

testPushPull :: IO ()
testPushPull = do
  let params = defaultIoUringParams
  withIoUring params $ \ctx -> do
    withReactor ctx $ \reactor -> do
      withContext $ \zmqCtx -> do
        
        r <- async $ do
          withSocket zmqCtx ZMQ.Pull $ \sock -> do
            setLinger (restrict (0 :: Int)) sock
            bind sock "inproc://test"
            msg <- asyncRecv reactor sock
            assertEqual "Message content" "hello" msg
            
        threadDelay 50000
        
        withSocket zmqCtx ZMQ.Push $ \sock -> do
          connect sock "inproc://test"
          asyncSend reactor sock "hello"
          
        wait r
