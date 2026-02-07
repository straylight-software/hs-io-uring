{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module System.IoUring.Test.Integration (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Property, ioProperty)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Unsafe as BSU
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Posix.IO (openFd, closeFd, OpenMode(ReadWrite), defaultFileFlags)
import Foreign (castPtr, copyBytes)

import System.IoUring
import System.IoUring.Reactor
import System.IoUring.Buffer

tests :: TestTree
tests = testGroup "Integration Tests"
  [ testProperty "File Write/Read Roundtrip" propWriteRead
  ]

propWriteRead :: String -> Property
propWriteRead content = ioProperty $ do
  let bs = BSC.pack content
  let len = BS.length bs
  
  if len == 0 
    then return True 
    else withSystemTempFile "io-uring-test" $ \fp h -> do
      hClose h
      
      fd <- openFd fp ReadWrite defaultFileFlags
      
      let params = defaultIoUringParams
      res <- withIoUring params $ \ctx -> do
        withReactor ctx $ \reactor -> do
          pool <- newBufferPool ctx 1 4096
          
          -- Write
          withBuffer pool $ \_ bufPtr -> do
             -- Copy data to buffer
             BSU.unsafeUseAsCStringLen bs $ \(cstr, clen) -> 
               copyBytes (castPtr bufPtr) (castPtr cstr) clen
             
             -- Write using WritePtrOp
             resW <- submitRequest reactor $ \push ->
                 push (WritePtrOp fd 0 (castPtr bufPtr) (fromIntegral len))
             
             case resW of
               Complete n -> if n == fromIntegral len then return () else fail "Short write"
               _ -> fail "Write failed"
               
             -- Read back
             resR <- submitRequest reactor $ \push ->
                 push (ReadPtrOp fd 0 (castPtr bufPtr) (fromIntegral len))
                   
             case resR of
               Complete n -> if n == fromIntegral len then return () else fail "Short read"
               _ -> fail "Read failed"
               
             -- Verify content
             bsRead <- BS.packCStringLen (castPtr bufPtr, len)
             
             return (bs == bsRead)
               
      closeFd fd
      return res
