-- FFI binding tests
module System.IoUring.Test.FFI where

import Foreign (Ptr)
import Foreign.C (CInt)
import Data.Word (Word32)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import System.IoUring.Internal.FFI (enterFlags, iosqeIoLink, msgDontwait, c_io_uring_queue_init, c_io_uring_queue_exit, c_io_uring_submit, c_io_uring_get_sqe)

tests :: TestTree
tests = testGroup "FFI Tests"
  [ testCase "Constants are defined" testConstants
  , testCase "FFI functions are importable" testFFIImports
  ]

testConstants :: Assertion
testConstants = do
  -- Just verify constants are defined and have reasonable values
  enterFlags @?= 8
  iosqeIoLink @?= 4
  msgDontwait @?= 64

testFFIImports :: Assertion
testFFIImports = do
  -- Verify FFI functions are importable by checking their types
  -- We can't actually call them without a valid io_uring, but we can verify they exist
  let _ = c_io_uring_queue_init :: CInt -> Ptr () -> Word32 -> IO CInt
      _ = c_io_uring_queue_exit :: Ptr () -> IO ()
      _ = c_io_uring_submit :: Ptr () -> IO CInt
      _ = c_io_uring_get_sqe :: Ptr () -> IO (Ptr ())
  return ()
