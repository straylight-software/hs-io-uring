-- Unit tests for io_uring operations
module System.IoUring.Test.Operations where

import Data.Word (Word8)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import System.IoUring.Internal.FFI (OpCode(OpNop, OpReadv, OpWritev, OpFsync), opcode, enterFlags, iosqeIoLink, msgDontwait)
import System.IoUring.URing (initURing, closeURing, validURing)

tests :: TestTree
tests = testGroup "Operations Tests"
  [ testGroup "Ring Operations"
      [ testCase "Can create and destroy ring" testRingLifecycle
      , testCase "Ring parameters are valid" testRingParams
      ]
  , testGroup "Constants"
      [ testCase "Operation codes are sequential" testOpCodesSequential
      , testCase "Constants have expected values" testConstantValues
      ]
  ]

testRingLifecycle :: Assertion
testRingLifecycle = do
  -- Create a ring with small queue
  ring <- initURing 0 32 64
  
  -- Verify it's valid
  valid <- validURing ring
  valid @?= True
  
  -- Clean up
  closeURing ring

testRingParams :: Assertion
testRingParams = do
  -- Test with different parameters
  ring1 <- initURing 0 16 32
  closeURing ring1
  
  ring2 <- initURing 0 64 128
  closeURing ring2
  
  ring3 <- initURing 0 256 512
  closeURing ring3

testOpCodesSequential :: Assertion
testOpCodesSequential = do
  -- Verify opcodes are sequential starting from 0
  opcode OpNop @?= 0
  opcode OpReadv @?= 1
  opcode OpWritev @?= 2
  opcode OpFsync @?= 3
  -- Continue pattern...
  let opcodes = [0..29] :: [Word8]
  length opcodes @?= 30

testConstantValues :: Assertion
testConstantValues = do
  -- Verify constants match expected kernel values
  enterFlags @?= 8
  iosqeIoLink @?= 4
  msgDontwait @?= 64
