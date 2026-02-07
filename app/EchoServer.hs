{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Exception (try, SomeException)
import Foreign (castPtr, Ptr)
import System.Posix.Types (Fd(Fd))
import Network.Socket (Socket, Family(AF_INET), SocketType(Stream), defaultProtocol, socket, bind, listen, close, setSocketOption, SocketOption(ReuseAddr), accept, SockAddr(SockAddrInet), unsafeFdSocket)
import System.IoUring (defaultIoUringParams, withIoUring, ioBatchSizeLimit, ioConcurrencyLimit, IoResult(Complete), IoOp(RecvPtrOp, SendZcPtrOp))
import System.IoUring.Reactor (Reactor, withReactor, submitRequest)
import System.IoUring.Buffer (BufferPool, newBufferPool, bufferSize, withBuffer)
import Foreign.C.Types (CInt)
import System.IoUring.Logging (runKatipContextT, logMsg, Severity(InfoS), KatipContextT)
import System.IoUring.Options (withOptions)

main :: IO ()
main = withOptions "echo-server" $ \_opts le -> runKatipContextT le () "main" $ do
  logMsg "startup" InfoS "Starting io_uring Reactor Echo Server on port 8080..."
  
  -- Create listening socket
  sock <- liftIO $ socket AF_INET Stream defaultProtocol
  liftIO $ setSocketOption sock ReuseAddr 1
  liftIO $ bind sock (SockAddrInet 8080 0)
  liftIO $ listen sock 128
  
  -- Initialize io_uring
  let params = defaultIoUringParams { ioBatchSizeLimit = 128, ioConcurrencyLimit = 256 }
  liftIO $ withIoUring params $ \ctx -> do
    -- We need to log from inside, but withIoUring runs in IO.
    -- We can use `runKatipContextT le ...` inside if we want, or just liftIO for setup.
    -- Better: run logging in main thread, and pass LogEnv.
    runKatipContextT le () "init" $ logMsg "init" InfoS "IoUring initialized."
    
    withReactor ctx $ \reactor -> do
      runKatipContextT le () "reactor" $ logMsg "init" InfoS "Reactor started."
      
      pool <- newBufferPool ctx 100 4096
      runKatipContextT le () "pool" $ logMsg "init" InfoS "Buffer Pool created."
      
      -- Accept loop
      forever $ do
        (connSock, _addr) <- liftIO $ accept sock
        fdVal <- liftIO $ unsafeFdSocket connSock
        let connFd = Fd fdVal
        
        -- Handle client in a new thread
        _ <- liftIO $ forkIO $ runKatipContextT le () "handler" $ handleClient reactor pool connSock connFd
        return ()

handleClient :: Reactor -> BufferPool -> Socket -> Fd -> KatipContextT IO ()
handleClient reactor pool sock fd = do
  -- Allocate a buffer from the pool
  -- withBuffer runs in IO. We need to lift.
  liftIO $ withBuffer pool $ \_bufId bufPtr -> do
    let ptr = castPtr bufPtr :: Ptr CInt 
        size = fromIntegral (bufferSize pool)
        
    let loop = do
          res <- submitRequest reactor $ \push -> 
              push (RecvPtrOp fd (castPtr ptr) size 0)
            
          case res of
            Complete n | n > 0 -> do
               sendRes <- submitRequest reactor $ \push ->
                   push (SendZcPtrOp fd (castPtr ptr) n 0 0)
                   
               case sendRes of
                 Complete sent | sent > 0 -> loop
                 _ -> return () 
                 
            Complete 0 -> return () 
            _ -> return () 
          
    _ <- try loop :: IO (Either SomeException ())
    return ()
    
  liftIO $ close sock

