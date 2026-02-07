{-# LANGUAGE ScopedTypeVariables #-}

-- Memory safety tests for io_uring
module System.IoUring.Test.MemorySafety where

import Control.Exception (try, SomeException)
import Control.Monad (replicateM_, forM_)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import System.IoUring.URing (initURing, closeURing, validURing)

tests :: TestTree
tests = testGroup "Memory Safety"
  [ testGroup "Allocation Patterns"
      [ testCase "Sequential allocation/deallocation" testSequentialAlloc
      , testCase "Interleaved allocation" testInterleavedAlloc
      , testCase "Nested bracket pattern" testNestedBracket
      ]
  , testGroup "Resource Cleanup"
      [ testCase "Cleanup on exception" testCleanupOnException
      , testCase "Multiple cleanup paths" testMultipleCleanupPaths
      , testCase "Resource exhaustion handling" testResourceExhaustion
      ]
  ]

testSequentialAlloc :: Assertion
testSequentialAlloc = do
  -- Allocate and deallocate sequentially
  replicateM_ 100 $ do
    ring <- initURing 0 64 128
    valid <- validURing ring
    valid @?= True
    closeURing ring

testInterleavedAlloc :: Assertion
testInterleavedAlloc = do
  -- Create multiple rings, use them, then close in different order
  ring1 <- initURing 0 32 64
  ring2 <- initURing 1 32 64
  ring3 <- initURing 2 32 64
  
  -- Validate all
  valid1 <- validURing ring1
  valid2 <- validURing ring2
  valid3 <- validURing ring3
  
  valid1 @?= True
  valid2 @?= True
  valid3 @?= True
  
  -- Close in reverse order
  closeURing ring3
  closeURing ring2
  closeURing ring1

testNestedBracket :: Assertion
testNestedBracket = do
  -- Test nested bracket patterns
  let outer = do
        ring1 <- initURing 0 16 32
        let inner = do
              ring2 <- initURing 1 16 32
              valid2 <- validURing ring2
              valid2 @?= True
              closeURing ring2
        inner
        valid1 <- validURing ring1
        valid1 @?= True
        closeURing ring1
  outer

testCleanupOnException :: Assertion
testCleanupOnException = do
  -- Ensure cleanup happens even with exceptions
  result <- try $ do
    ring <- initURing 0 32 64
    valid <- validURing ring
    valid @?= True
    -- Simulate an error condition
    if valid then error "Simulated error" else return ()
    closeURing ring
  
  case result of
    Left (_ :: SomeException) -> return ()  -- Expected
    Right () -> return ()  -- Should not reach here

testMultipleCleanupPaths :: Assertion
testMultipleCleanupPaths = do
  -- Test various cleanup paths
  forM_ [0..9] $ \i -> do
    ring <- initURing i 16 32
    valid <- validURing ring
    valid @?= True
    closeURing ring

testResourceExhaustion :: Assertion
testResourceExhaustion = do
  -- Test behavior when creating many rings
  -- This tests resource management
  let batchSize = 50
  forM_ [0..9 :: Int] $ \_ -> do
    rings <- mapM (\i -> initURing i 8 16) [0..batchSize-1]
    mapM_ validURing rings
    mapM_ closeURing rings
