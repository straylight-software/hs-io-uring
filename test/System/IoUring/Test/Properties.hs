-- Property-based tests for io_uring
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-orphans #-}
module System.IoUring.Test.Properties where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(arbitrary), elements)

import System.IoUring.Internal.FFI (OpCode, opcode, enterFlags, iosqeIoLink, msgDontwait)

tests :: TestTree
tests = testGroup "Property Tests"
  [ testProperty "OpCode roundtrip" propOpCodeRoundtrip
  , testProperty "Constants are positive" propConstantsPositive
  ]

instance Arbitrary OpCode where
  arbitrary = elements [minBound .. maxBound]

propOpCodeRoundtrip :: OpCode -> Bool
propOpCodeRoundtrip op = fromEnum op == fromIntegral (opcode op)

propConstantsPositive :: Bool
propConstantsPositive =
  enterFlags > 0 &&
  iosqeIoLink > 0 &&
  msgDontwait > 0
