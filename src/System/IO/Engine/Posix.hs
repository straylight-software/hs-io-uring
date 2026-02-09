{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module System.IO.Engine.Posix
  ( makePosixEngine
  ) where

import System.IO.Engine.Types
  ( Engine(Engine, submit, submitBatch, shutdown, backendName)
  , Request(Read, Write, Readv, Writev, ReadAt, WriteAt, Accept, Connect, Close, WaitRead, WaitWrite, Fsync)
  , Ticket
  )
import Control.Concurrent (threadWaitRead, threadWaitWrite)
import Control.Concurrent.Async (async)
import System.Posix.IO.ByteString (fdRead, fdWrite, fdSeek)
import System.IO (SeekMode(AbsoluteSeek))
import System.Posix.Types (Fd(Fd))
import System.Posix.IO (closeFd, dup)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Data.Word (Word8)
import qualified Network.Socket as Net
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.ByteString.Internal as BSI
import Foreign.ForeignPtr (newForeignPtr_, castForeignPtr)

-- | Creates an Engine backed by GHC's IO manager (epoll/kqueue).
-- This acts as a reference implementation and fallback.
makePosixEngine :: IO Engine
makePosixEngine = return Engine
  { submit = submitPosix
  , submitBatch = mapM submitPosix -- Posix doesn't really batch
  , shutdown = return ()
  , backendName = "posix"
  }

submitPosix :: forall a. Request a -> IO (Ticket a)
submitPosix req = async $ execute req

execute :: Request a -> IO a
execute = \case
  Read (Fd fd) ptr len -> do
    -- GHC's threadWaitRead uses the IO manager (epoll)
    threadWaitRead (Fd fd) 
    -- We need to read into the Ptr. GHC's unix library provides fdReadBuf
    -- but System.Posix.IO.ByteString is higher level. 
    -- For efficiency in this "Simple" backend, we might just copy for now
    -- or use the FFI directly if we wanted to be strictly zero-copy.
    -- Given "Simple" means robust:
    bytes <- fdRead (Fd fd) (fromIntegral len)
    BSU.unsafeUseAsCStringLen bytes $ \(src, l) -> 
      copyBytes ptr (castPtr src) l
    return (BS.length bytes)

  Write (Fd fd) ptr len -> do
    threadWaitWrite (Fd fd)
    -- Reconstruct BS from Ptr (Unsafe but fast)
    fp <- newForeignPtr_ ptr
    let bs = BSI.fromForeignPtr (castForeignPtr fp) 0 len
    count <- fdWrite (Fd fd) bs
    return (fromIntegral count)

  WaitRead fd -> threadWaitRead fd
  
  WaitWrite fd -> threadWaitWrite fd

  Accept fd -> do
    threadWaitRead fd
    -- We need to accept. Network.Socket.accept takes a Socket.
    -- To avoid closing the original FD when the wrapper is GC'd, we dup it?
    -- Actually, for Accept, the 'sock' is the listening socket.
    -- If we close it, we stop listening.
    -- We must dup!
    fd' <- dup fd
    sock <- mkSocket fd'
    (clientSock, addr) <- Net.accept sock
    -- The wrapper 'sock' will be GC'd and close fd'. Original fd stays open.
    
    -- Now clientSock is a new socket. We want to return its FD and detach it from GC closing?
    -- If we just return the FD, and clientSock dies, it closes the new connection!
    -- We must dup the client FD too!
    clientFdRaw <- getFd clientSock
    clientFdDup <- dup clientFdRaw
    -- clientSock will eventually GC and close clientFdRaw.
    -- We return clientFdDup.
    return (clientFdDup, addr)

  Connect fd addr -> do
    -- connect closes the socket on error? No.
    -- But the wrapper will close it on GC.
    fd' <- dup fd
    sock <- mkSocket fd'
    Net.connect sock addr
    -- sock GC's, closes fd'. Original fd is connected and open.

  Close (Fd fd) -> closeFd (Fd fd)

  ReadAt (Fd fd) ptr len off -> do
    -- Pread emulation (racy if shared, but this is Simple backend)
    -- A real implementation would use FFI 'pread' to avoid seek races
    _ <- fdSeek (Fd fd) AbsoluteSeek (fromIntegral off)
    bytes <- fdRead (Fd fd) (fromIntegral len)
    BSU.unsafeUseAsCStringLen bytes $ \(src, l) -> 
      copyBytes ptr (castPtr src) l
    return (BS.length bytes)
    
  WriteAt (Fd fd) ptr len off -> do
    _ <- fdSeek (Fd fd) AbsoluteSeek (fromIntegral off)
    fp <- newForeignPtr_ ptr
    let bs = BSI.fromForeignPtr (castForeignPtr fp) 0 len
    count <- fdWrite (Fd fd) bs
    return (fromIntegral count)

  -- Stubs for now
  Readv {} -> error "Posix Readv not impl"
  Writev {} -> error "Posix Writev not impl"
  Fsync (Fd _) -> return () -- effectively no-op or fsync(fd)

-- Utils

mkSocket :: Fd -> IO Net.Socket
mkSocket fd = do
    -- network >= 3.0 uses Fd instead of CInt for mkSocket
    Net.mkSocket (fromIntegral fd)

getFd :: Net.Socket -> IO Fd
getFd s = Fd <$> Net.socketToFd s

copyBytes :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
copyBytes dst src len = do
  -- This is slow, strict peek/poke. 
  -- In a real impl we'd use memcpy via FFI
  bytes <- peekArray len src
  pokeArray dst bytes