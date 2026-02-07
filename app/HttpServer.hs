{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Exception (bracket)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Foreign (castPtr, Ptr, copyBytes, mallocBytes, free)
import System.Posix.Types (Fd(Fd))
import Network.Socket (Socket, Family(AF_INET), SocketType(Stream), defaultProtocol, socket, bind, listen, close, setSocketOption, SocketOption(ReuseAddr), accept, SockAddr(SockAddrInet), unsafeFdSocket)
import Data.Word (Word8)

import System.IoUring (defaultIoUringParams, withIoUring, ioBatchSizeLimit, ioConcurrencyLimit, IoResult(Complete), IoOp(RecvPtrOp, SendZcPtrOp))
import System.IoUring.Reactor (Reactor, withReactor, submitRequest)
import System.IoUring.Buffer (BufferPool, newBufferPool, bufferSize, withBuffer)
import System.IoUring.Logging (runKatipContextT, logMsg, Severity(InfoS), KatipContextT)
import System.IoUring.Options (withOptions)

-- | Simple HTTP Response
response :: BS.ByteString
response = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 13\r\n\r\nHello, World!"

main :: IO ()
main = withOptions "http-server" $ \_opts le -> runKatipContextT le () "main" $ do
  logMsg "startup" InfoS "Starting io_uring HTTP Server on port 8080..."

  
  sock <- liftIO $ socket AF_INET Stream defaultProtocol
  liftIO $ setSocketOption sock ReuseAddr 1
  liftIO $ bind sock (SockAddrInet 8080 0)
  liftIO $ listen sock 1024
  
  -- Initialize io_uring
  let params = defaultIoUringParams { ioBatchSizeLimit = 128, ioConcurrencyLimit = 256 }
  liftIO $ withIoUring params $ \ctx -> do
    withReactor ctx $ \reactor -> do
      -- Create Buffer Pool
      pool <- newBufferPool ctx 100 4096
      
      -- Pre-allocate response buffer to demonstrate Zero-Copy from pinned memory
      let respLen = BS.length response
      bracket (mallocBytes respLen) free $ \respPtr -> do
        -- Copy static response to pinned memory
        BSU.unsafeUseAsCString response $ \cstr -> 
            copyBytes respPtr (castPtr cstr) respLen
            
        -- Accept loop
        forever $ do
          (connSock, _) <- accept sock
          connFdVal <- unsafeFdSocket connSock
          let connFd = Fd connFdVal
          _ <- forkIO $ runKatipContextT le () "handler" $ handleClient reactor pool connFd connSock (castPtr respPtr) respLen
          return ()

handleClient :: Reactor -> BufferPool -> Fd -> Socket -> Ptr Word8 -> Int -> KatipContextT IO ()
handleClient reactor pool fd sock respPtr respLen = do
  -- Alloc buffer for request
  liftIO $ withBuffer pool $ \_ bufPtr -> do
    let ptr = castPtr bufPtr :: Ptr Word8
        size = fromIntegral (bufferSize pool)
    
    -- Read Request (RecvPtrOp)
    -- We just read once and assume it contains the full GET line for this demo
    res <- submitRequest reactor $ \push ->
        push (RecvPtrOp fd ptr size 0)
        
    case res of
      Complete n | n > 0 -> do
        -- Check if it looks like a GET (very primitive check)
        -- In reality we would parse `ptr`
        -- For this demo, we just unconditionally send the response
        
        -- Send Response (Zero Copy)
        _ <- submitRequest reactor $ \push ->
            push (SendZcPtrOp fd respPtr (fromIntegral respLen) 0 0)
            
        return ()
      _ -> return ()
      
  liftIO $ close sock
