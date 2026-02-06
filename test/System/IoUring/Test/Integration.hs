-- Integration tests for io_uring
module System.IoUring.Test.Integration where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import System.IoUring (defaultIoUringParams, withIoUring)
import System.IoUring.URing (initURing, closeURing, validURing)

tests :: TestTree
tests = testGroup "Integration Tests"
  [ testGroup "Ring Lifecycle"
      [ testCase "Create and close ring" testCreateCloseRing
      , testCase "Multiple rings can coexist" testMultipleRings
      ]
  , testGroup "Basic Operations"
      [ testCase "Submit empty batch" testEmptyBatch
      , testCase "Ring validation" testRingValidation
      ]
  ]

testCreateCloseRing :: Assertion
testCreateCloseRing = do
  ring <- initURing 0 32 64
  valid <- validURing ring
  valid @?= True
  closeURing ring

testMultipleRings :: Assertion
testMultipleRings = do
  let createRing i = do
        ring <- initURing i 32 64
        valid <- validURing ring
        valid @?= True
        return ring
  
  rings <- mapM createRing [0..3]
  mapM_ closeURing rings

testEmptyBatch :: Assertion
testEmptyBatch = do
  withIoUring defaultIoUringParams $ \_ctx -> do
    return ()

testRingValidation :: Assertion
testRingValidation = do
  ring <- initURing 0 32 64
  valid <- validURing ring
  valid @?= True
  closeURing ring
