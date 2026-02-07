{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module System.IoUring.Test.Buffer (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual)
import System.IoUring
import System.IoUring.Buffer

tests :: TestTree
tests = testGroup "Buffer Pool Tests"
  [ testCase "Buffer Pool Creation" testCreation
  , testCase "Buffer Allocation" testAllocation
  ]

testCreation :: IO ()
testCreation = do
  let params = defaultIoUringParams
  withIoUring params $ \ctx -> do
    pool <- newBufferPool ctx 10 1024
    assertEqual "Buffer size correct" 1024 (bufferSize pool)
    freeBufferPool pool

testAllocation :: IO ()
testAllocation = do
  let params = defaultIoUringParams
  withIoUring params $ \ctx -> do
    pool <- newBufferPool ctx 2 1024
    
    -- Alloc 1
    id1 <- allocBuffer pool
    -- Alloc 2
    id2 <- allocBuffer pool
    
    assertBool "IDs different" (id1 /= id2)
    
    -- Release
    releaseBuffer pool id1
    releaseBuffer pool id2
    
    freeBufferPool pool
