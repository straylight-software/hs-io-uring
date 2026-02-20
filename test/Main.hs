-- Main test suite for libevring library
-- Comprehensive tests covering FFI, operations, and integration

module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified System.IoUring.Test.FFI
import qualified System.IoUring.Test.Operations
import qualified System.IoUring.Test.Properties
import qualified System.IoUring.Test.Integration
import qualified System.IoUring.Test.Concurrency
import qualified Evring.Test.Sigil

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "libevring Test Suite"
  [ System.IoUring.Test.FFI.tests
  , System.IoUring.Test.Operations.tests
  , System.IoUring.Test.Properties.tests
  , System.IoUring.Test.Integration.tests
  , System.IoUring.Test.Concurrency.tests
  , Evring.Test.Sigil.tests
  ]
