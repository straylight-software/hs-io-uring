-- Manual FFI bindings for io_uring (no hsc)
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DerivingStrategies #-}

module System.IoUring.URing
  ( URing(..)
  , URingParams(..)
  , initURing
  , closeURing
  , cleanupURing
  , validURing
  , submitIO
  , awaitIO
  , peekIO
  , IOCompletion(..)
  , IOResult(..)
  , IOOpId(..)
  ) where

import Foreign (Ptr, callocBytes, free, alloca, peek, peekByteOff)
import Data.Word (Word32, Word64)
import Data.Int (Int32, Int64)

import System.IoUring.Internal.FFI (
    c_io_uring_queue_init, 
    c_io_uring_queue_exit, 
    c_io_uring_submit,
    c_hs_uring_wait_cqe,
    c_hs_uring_peek_cqe,
    c_hs_uring_cqe_seen
  )

-- ============================================================================
-- TYPES
-- ============================================================================

newtype IOResult = IOResult Int64
  deriving stock Show

newtype IOOpId = IOOpId Word64
  deriving stock (Show, Eq)

data IOCompletion = IOCompletion {
    completionId   :: !IOOpId,
    completionRes  :: !IOResult
  }
  deriving stock Show

data URingParams = URingParams {
    uringSqEntries :: !Word32,
    uringCqEntries :: !Word32,
    uringFlags     :: !Word32
  } deriving stock Show

-- ============================================================================
-- FFI WRAPPERS
-- ============================================================================

data URing = URing {
    uRingPtr     :: !(Ptr ())
  }

initURing :: Int -> Int -> Int -> IO URing
initURing _capNo sqEntries _cqEntries = do
  ptr <- callocBytes 4096  -- Conservative estimate for io_uring struct (increased for safety)
  ret <- c_io_uring_queue_init (fromIntegral sqEntries) ptr 0
  if ret < 0
    then do
      free ptr
      ioError $ userError "io_uring_queue_init failed"
    else return $ URing ptr

closeURing :: URing -> IO ()
closeURing (URing ptr) = do
  c_io_uring_queue_exit ptr
  free ptr

cleanupURing :: URing -> IO ()
cleanupURing = closeURing

validURing :: URing -> IO Bool
validURing _ = return True

-- ============================================================================
-- SUBMISSION
-- ============================================================================

submitIO :: URing -> IO ()
submitIO (URing ptr) = do
  ret <- c_io_uring_submit ptr
  if ret < 0
    then ioError $ userError $ "io_uring_submit failed: " ++ show ret
    else return ()

-- ============================================================================
-- COMPLETIONS
-- ============================================================================

awaitIO :: URing -> IO IOCompletion
awaitIO (URing ringPtr) = alloca $ \cqePtrPtr -> do
  res <- c_hs_uring_wait_cqe ringPtr cqePtrPtr
  if res < 0
    then ioError $ userError $ "io_uring_wait_cqe failed: " ++ show res
    else do
      cqePtr <- peek cqePtrPtr
      userData <- peekByteOff cqePtr 0 :: IO Word64
      res32    <- peekByteOff cqePtr 8 :: IO Int32
      
      c_hs_uring_cqe_seen ringPtr cqePtr
      
      return $ IOCompletion (IOOpId userData) (IOResult (fromIntegral res32))

peekIO :: URing -> IO (Maybe IOCompletion)
peekIO (URing ringPtr) = alloca $ \cqePtrPtr -> do
  res <- c_hs_uring_peek_cqe ringPtr cqePtrPtr
  if res == 0 -- 0 means success (found cqe)
    then do
      cqePtr <- peek cqePtrPtr
      userData <- peekByteOff cqePtr 0 :: IO Word64
      res32    <- peekByteOff cqePtr 8 :: IO Int32
      
      c_hs_uring_cqe_seen ringPtr cqePtr
      
      return $ Just $ IOCompletion (IOOpId userData) (IOResult (fromIntegral res32))
    else return Nothing

-- Constants are re-exported from FFI module
