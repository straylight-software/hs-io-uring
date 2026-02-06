-- Minimal stub for Socket.Batch module
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}

module System.IoUring.Socket.Batch
  ( SockCtx
  , SockCtxParams(..)
  , defaultSockCtxParams
  , withSockCtx
  , initSockCtx
  , closeSockCtx
  , SockOp(..)
  , SockResult(..)
  , submitSockOps
  ) where

import Foreign (Ptr)
import Foreign.C.Error (Errno(Errno))
import System.Posix.Types (Fd, ByteCount)
import Data.Word (Word8, Word32)
import Control.Exception (bracket)

-- Stub types
data SockCtx = SockCtx

data SockCtxParams = SockCtxParams {
    sockCtxBatchSizeLimit :: !Int,
    sockCtxConcurrencyLimit :: !Int
  }

defaultSockCtxParams :: SockCtxParams
defaultSockCtxParams = SockCtxParams 64 192

withSockCtx :: SockCtxParams -> (SockCtx -> IO a) -> IO a
withSockCtx params = bracket (initSockCtx params) closeSockCtx

initSockCtx :: SockCtxParams -> IO SockCtx
initSockCtx _ = return SockCtx

closeSockCtx :: SockCtx -> IO ()
closeSockCtx _ = return ()

-- GADT for socket operations
data SockOp a where
  SockRecv :: !Fd -> Ptr Word8 -> ByteCount -> Word32 -> SockOp ByteCount
  SockSend :: !Fd -> Ptr Word8 -> ByteCount -> Word32 -> SockOp ByteCount

data SockResult a = SockSuccess a | SockError Errno

instance Show a => Show (SockResult a) where
  show (SockSuccess x) = "SockSuccess " ++ show x
  show (SockError (Errno e)) = "SockError " ++ show e

-- Stub function
submitSockOps :: SockCtx -> [SockOp a] -> IO [SockResult a]
submitSockOps _ _ = return []
