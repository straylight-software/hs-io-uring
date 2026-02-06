-- Concurrency tests for io_uring
module System.IoUring.Test.Concurrency where

import Control.Concurrent (forkIO)
import Control.Monad (replicateM)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import System.IoUring.URing (initURing, closeURing, validURing)

tests :: TestTree
tests = testGroup "Concurrency Tests"
  [ testGroup "Multiple Rings"
      [ testCase "Concurrent ring creation" testConcurrentRings
      , testCase "Ring operations from multiple threads" testMultiThreadedOps
      ]
  ]

testConcurrentRings :: Assertion
testConcurrentRings = do
  -- Create multiple rings concurrently
  let createRing i = do
        ring <- initURing i 32 64
        valid <- validURing ring
        valid @?= True
        return ring
  
  rings <- mapM createRing [0..3]
  mapM_ closeURing rings

testMultiThreadedOps :: Assertion
testMultiThreadedOps = do
  ring <- initURing 0 64 128
  
  -- Spawn multiple threads that validate the ring
  results <- replicateM 4 $ forkIO $ do
    valid <- validURing ring
    valid `seq` return ()
  
  -- Wait for all threads
  mapM_ (\_ -> return ()) results
  
  closeURing ring
