{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.IoUring.ZMQ
  ( asyncRecv
  , asyncSend
  , waitZmq
  ) where

import qualified Data.ByteString as BS
import System.ZMQ4 (Socket, Sender, Receiver, Event(In, Out))
import qualified System.ZMQ4 as ZMQ

import System.IoUring (IoOp(PollAddOp))
import System.IoUring.Reactor (Reactor, submitRequest)

-- | Wait for a ZMQ socket to become readable or writable using io_uring.
-- This uses the underlying ZMQ FD.
waitZmq :: Reactor -> Socket a -> Event -> IO ()
waitZmq reactor sock evt = do
  -- Get the internal ZMQ file descriptor
  -- fileDescriptor returns System.Posix.Types.Fd
  fd <- ZMQ.fileDescriptor sock
  
  -- Map ZMQ event to poll mask
  let mask = case evt of
        In  -> 1 -- POLLIN
        Out -> 4 -- POLLOUT
        _   -> 1 -- Default to POLLIN
  
  -- Submit PollAddOp
  -- We don't really care about the result value, just that it returns
  _ <- submitRequest reactor $ \push ->
      push (PollAddOp fd mask)
      
  return ()

-- | Receive a message asynchronously.
-- Tries to receive non-blocking. If EAGAIN, waits on reactor and retries.
asyncRecv :: Receiver a => Reactor -> Socket a -> IO BS.ByteString
asyncRecv reactor sock = loop
  where
    loop = do
      evs <- ZMQ.events sock
      if In `elem` evs
        then ZMQ.receive sock
        else do
          waitZmq reactor sock In
          loop

-- | Send a message asynchronously.
asyncSend :: Sender a => Reactor -> Socket a -> BS.ByteString -> IO ()
asyncSend reactor sock msg = loop
  where
    loop = do
      evs <- ZMQ.events sock
      if Out `elem` evs
        then ZMQ.send sock [] msg
        else do
          waitZmq reactor sock Out
          loop
