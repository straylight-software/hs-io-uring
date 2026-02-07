{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad (forever)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BSC
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types as H
import System.ZMQ4 (Context, Socket, Dealer, withContext, withSocket, bind, connect, setIdentity)
import qualified System.ZMQ4 as ZMQ
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

import System.IoUring (defaultIoUringParams, withIoUring, ioBatchSizeLimit, ioConcurrencyLimit)
import System.IoUring.Reactor (Reactor, withReactor)
import System.IoUring.ZMQ (asyncRecv, asyncSend)
import System.IoUring.Logging (runKatipContextT, logMsg, Severity(InfoS), LogEnv)
import System.IoUring.Options (withOptions)
import Katip (ls)

main :: IO ()
main = withOptions "http2-proxy" $ \_opts le -> runKatipContextT le () "main" $ do
  liftIO $ hSetBuffering stdout NoBuffering
  logMsg "startup" InfoS "Starting HTTP/2 -> ZMQ Proxy Demo"

  
  -- Initialize io_uring
  let params = defaultIoUringParams { ioBatchSizeLimit = 128, ioConcurrencyLimit = 256 }
  logMsg "init" InfoS "Initializing io_uring..."
  liftIO $ withIoUring params $ \ctx -> do
    -- Log from IO context using le
    runKatipContextT le () "init" $ logMsg "init" InfoS "io_uring initialized. Initializing Reactor..."
    withReactor ctx $ \reactor -> do
      runKatipContextT le () "init" $ logMsg "init" InfoS "Reactor initialized. Initializing ZMQ..."
      withContext $ \zctx -> do
        runKatipContextT le () "init" $ logMsg "init" InfoS "ZMQ Context created."
        
        -- Start Dummy ZMQ Backend (ROUTER)
        _ <- forkIO $ runBackend le zctx
        
        -- Give backend time to start
        threadDelay 100000
        
        -- Start Proxy ZMQ Client (DEALER)
        runKatipContextT le () "init" $ logMsg "init" InfoS "Creating Dealer socket..."
        withSocket zctx ZMQ.Dealer $ \sock -> do
          setIdentity (ZMQ.restrict "proxy") sock
          connect sock "inproc://backend"
          runKatipContextT le () "init" $ logMsg "init" InfoS "Dealer connected."
          
          sockVar <- newMVar sock
          
          -- Start Warp Server
          runKatipContextT le () "init" $ logMsg "init" InfoS "Listening on http://localhost:8080 (HTTP/2 supported)"
          Warp.run 8080 (app le reactor sockVar)

-- | Dummy Backend Service
runBackend :: LogEnv -> Context -> IO ()
runBackend le zctx = do
  withSocket zctx ZMQ.Router $ \sock -> do
    bind sock "inproc://backend"
    runKatipContextT le () "backend" $ logMsg "backend" InfoS "Backend ready."
    forever $ do
      -- Router receives: [Identity, Message] from DEALER
      ident <- ZMQ.receive sock
      msg <- ZMQ.receive sock
      
      runKatipContextT le () "backend" $ logMsg "backend" InfoS $ ls ("Backend received: " ++ BSC.unpack msg)
      
      -- Echo back: Identity, Msg
      ZMQ.send sock [ZMQ.SendMore] ident
      ZMQ.send sock [] ("Echo: " `BSC.append` msg)

-- | WAI Application
app :: LogEnv -> Reactor -> MVar (Socket Dealer) -> Wai.Application
app le reactor sockVar req respond = do
  -- Read body
  body <- Wai.strictRequestBody req
  let msg = BL.toStrict body
  let msg' = if BSC.null msg then "Hello from HTTP/2" else msg
  
  -- Forward to ZMQ
  responseMsg <- withMVar sockVar $ \sock -> do
    runKatipContextT le () "app" $ logMsg "proxy" InfoS "Forwarding request to ZMQ"
    -- Send
    asyncSend reactor sock msg'
    -- Recv
    asyncRecv reactor sock
    
  respond $ Wai.responseLBS
    H.status200
    [("Content-Type", "text/plain")]
    (BL.fromStrict responseMsg)

