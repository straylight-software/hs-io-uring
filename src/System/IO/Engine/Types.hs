{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.IO.Engine.Types
  ( -- * Abstract Request
    Request(..)
  , Ticket
    -- * Engine Interface
  , Engine(..)
  , Backend(..)
  , EngineConfig(..)
  , defaultEngineConfig
  ) where

import System.Posix.Types (Fd)
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Network.Socket (SockAddr)
import Control.Concurrent.Async (Async)

-- | A "Ticket" represents a handle to a submitted operation.
-- It can be awaited or cancelled.
type Ticket a = Async a

-- | The core GADT defining all supported I/O operations.
-- This abstracts over the specific mechanism (io_uring vs epoll).
data Request a where
  -- | Standard Stream Read (Sockets, Pipes, TTYs)
  Read       :: Fd -> Ptr Word8 -> Int -> Request Int
  
  -- | Standard Stream Write
  Write      :: Fd -> Ptr Word8 -> Int -> Request Int
  
  -- | Vector Read (Scatter)
  Readv      :: Fd -> [(Ptr Word8, Int)] -> Request Int
  
  -- | Vector Write (Gather)
  Writev     :: Fd -> [(Ptr Word8, Int)] -> Request Int

  -- | Positional Read (Files)
  ReadAt     :: Fd -> Ptr Word8 -> Int -> Int -> Request Int

  -- | Positional Write (Files)
  WriteAt    :: Fd -> Ptr Word8 -> Int -> Int -> Request Int

  -- | Socket Accept
  Accept     :: Fd -> Request (Fd, SockAddr)
  
  -- | Socket Connect
  Connect    :: Fd -> SockAddr -> Request ()
  
  -- | Close a file descriptor
  Close      :: Fd -> Request ()

  -- | Reactor Primitive: Wait for Read readiness (Required for ZMQ)
  WaitRead   :: Fd -> Request ()
  
  -- | Reactor Primitive: Wait for Write readiness (Required for ZMQ)
  WaitWrite  :: Fd -> Request ()
  
  -- | File Sync
  Fsync      :: Fd -> Request ()

deriving stock instance Show (Request a)

-- | Abstract interface for an I/O Engine.
data Engine = Engine
  { -- | Submit a request. Returns a Ticket (Async) that can be awaited.
    submit :: forall a. Request a -> IO (Ticket a)
    
    -- | Submit a batch of requests. Optimizes submission overhead.
    -- Returns a list of Tickets in the same order.
  , submitBatch :: forall a. [Request a] -> IO [Ticket a]
  
    -- | Gracefully shutdown the engine
  , shutdown :: IO ()
  
    -- | Name of the active backend
  , backendName :: String
  }

data Backend = IO_URING | POSIX
  deriving (Show, Eq, Read)

data EngineConfig = EngineConfig
  { backend :: Maybe Backend -- ^ Force a specific backend (Nothing = Auto)
  , queueDepth :: Int        -- ^ Depth for io_uring (ignored by posix)
  } deriving (Show, Eq)

defaultEngineConfig :: EngineConfig
defaultEngineConfig = EngineConfig
  { backend = Nothing
  , queueDepth = 128
  }
