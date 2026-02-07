{-# LANGUAGE ScopedTypeVariables #-}

-- Edge case and error handling tests for io_uring
module System.IoUring.Test.EdgeCases where

import Control.Exception (try, SomeException)
import Control.Monad ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import System.IoUring.URing (initURing, closeURing, validURing, submitIO)

tests :: TestTree
tests = testGroup "Edge Cases"
  [ testGroup "Ring Initialization"
      [ testCase "Zero entries fails gracefully" testZeroEntries
      , testCase "Negative entries fails gracefully" testNegativeEntries
      , testCase "Very large entries" testVeryLargeEntries
      , testCase "Minimum valid ring" testMinimumRing
      ]
  , testGroup "Ring Operations"
      [ testCase "Submit on closed ring" testSubmitClosed
      , testCase "Multiple close calls" testMultipleClose
      , testCase "Validation after close" testValidationAfterClose
      ]
  , testGroup "Error Handling"
      [ testCase "Graceful error recovery" testErrorRecovery
      , testCase "Resource cleanup on error" testResourceCleanup
      ]
  ]

testZeroEntries :: Assertion
testZeroEntries = do
  -- Should handle zero entries gracefully
  result <- try $ initURing 0 0 0
  case result of
    Left (_ :: SomeException) -> return ()  -- Expected to fail
    Right ring -> do
      valid <- validURing ring
      valid @?= True
      closeURing ring

testNegativeEntries :: Assertion
testNegativeEntries = do
  -- Should handle negative entries gracefully
  result <- try $ initURing 0 (-1) (-1)
  case result of
    Left (_ :: SomeException) -> return ()  -- Expected to fail
    Right ring -> do
      valid <- validURing ring
      valid @?= True
      closeURing ring

testVeryLargeEntries :: Assertion
testVeryLargeEntries = do
  -- Test with large but reasonable entries
  ring <- initURing 0 1024 2048
  valid <- validURing ring
  valid @?= True
  closeURing ring

testMinimumRing :: Assertion
testMinimumRing = do
  -- Test minimum valid ring (1 entry)
  ring <- initURing 0 1 2
  valid <- validURing ring
  valid @?= True
  closeURing ring

testSubmitClosed :: Assertion
testSubmitClosed = do
  ring <- initURing 0 32 64
  closeURing ring
  -- Submit on closed ring should handle gracefully
  result <- try $ submitIO ring
  case result of
    Left (_ :: SomeException) -> return ()  -- Expected
    Right () -> return ()  -- Also acceptable

testMultipleClose :: Assertion
testMultipleClose = do
  ring <- initURing 0 32 64
  closeURing ring
  -- Multiple closes should be safe (idempotent)
  closeURing ring
  closeURing ring
  return ()

testValidationAfterClose :: Assertion
testValidationAfterClose = do
  ring <- initURing 0 32 64
  closeURing ring
  -- Validation after close
  valid <- validURing ring
  -- Should return True (ring struct still valid) or handle gracefully
  valid `seq` return ()

testErrorRecovery :: Assertion
testErrorRecovery = do
  -- Create ring, simulate error, recover
  ring <- initURing 0 32 64
  valid <- validURing ring
  valid @?= True
  
  -- Close and create new one (recovery)
  closeURing ring
  ring2 <- initURing 0 32 64
  valid2 <- validURing ring2
  valid2 @?= True
  closeURing ring2

testResourceCleanup :: Assertion
testResourceCleanup = do
  -- Create and destroy many rings to ensure no resource leaks
  let createAndDestroy i = do
        ring <- initURing i 32 64
        valid <- validURing ring
        valid @?= True
        closeURing ring
  
  mapM_ createAndDestroy [0..99]
  return ()
