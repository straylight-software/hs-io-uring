-- Main test suite for io-uring library
-- Comprehensive tests covering FFI, operations, and integration

module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified System.IoUring.Test.FFI
import qualified System.IoUring.Test.Operations
import qualified System.IoUring.Test.Properties
import qualified System.IoUring.Test.Integration
import qualified System.IoUring.Test.Concurrency
import qualified System.IoUring.Test.EdgeCases
import qualified System.IoUring.Test.Stress
import qualified System.IoUring.Test.MemorySafety
import qualified System.IoUring.Test.Reactor
import qualified System.IoUring.Test.Buffer
import qualified System.IoUring.Test.ZMQ

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "io-uring Test Suite"
  [ System.IoUring.Test.FFI.tests
  , System.IoUring.Test.Operations.tests
  , System.IoUring.Test.Properties.tests
  , System.IoUring.Test.Integration.tests
  , System.IoUring.Test.Concurrency.tests
  , System.IoUring.Test.EdgeCases.tests
  , System.IoUring.Test.Stress.tests
  , System.IoUring.Test.MemorySafety.tests
  , System.IoUring.Test.Reactor.tests
  , System.IoUring.Test.Buffer.tests
  , System.IoUring.Test.ZMQ.tests
  ]
