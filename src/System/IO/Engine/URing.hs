{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module System.IO.Engine.URing
  ( makeURingEngine
  ) where

import System.IO.Engine.Types
  ( Engine(Engine, submit, submitBatch, shutdown, backendName)
  , Request(Read, Write, Connect, Close)
  , Ticket
  )
import qualified System.IoUring as URing
import qualified Data.Vector as V
import Control.Concurrent.Async (async)
import Foreign.Ptr (castPtr)
import qualified Network.Socket as Net
import System.Posix.IO (dup)
import System.Posix.Types (Fd(Fd))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO (Handle)

makeURingEngine :: IO Engine
makeURingEngine = do
    -- Initialize the URing context
    -- We use default params for now, could be configurable
    ctx <- URing.initIoUring URing.defaultIoUringParams
    
    -- Store context in IORef to handle shutdown (idempotency)
    ctxRef <- newIORef (Just ctx)

    return Engine
      { submit = submitURing ctxRef
      , submitBatch = submitBatchURing ctxRef
      , shutdown = shutdownURing ctxRef
      , backendName = "io_uring"
      }

submitURing :: IORef (Maybe URing.IOCtx) -> Request a -> IO (Ticket a)
submitURing ctxRef req = async $ do
    mCtx <- readIORef ctxRef
    case mCtx of
      Nothing -> error "Engine shutdown"
      Just ctx -> executeOne ctx req

submitBatchURing :: IORef (Maybe URing.IOCtx) -> [Request a] -> IO [Ticket a]
submitBatchURing ctxRef reqs = do
    -- For now, we just spawn individual asyncs. 
    mapM (submitURing ctxRef) reqs

shutdownURing :: IORef (Maybe URing.IOCtx) -> IO ()
shutdownURing ctxRef = do
    mCtx <- readIORef ctxRef
    case mCtx of
      Nothing -> return ()
      Just ctx -> do
          URing.closeIoUring ctx
          writeIORef ctxRef Nothing

executeOne :: URing.IOCtx -> Request a -> IO a
executeOne ctx req = do
    -- Helper to run a single op batch and get the byte count result
    let runOp op = do
           results <- URing.submitBatch ctx $ \prep -> prep op
           if V.null results 
               then error "No result from batch"
               else case V.head results of
                   URing.Complete n -> return n
                   URing.IoErrno (URing.Errno e) -> ioError (errnoToIOError "io_uring" (URing.Errno e) Nothing Nothing)
                   URing.Eof -> return 0

    case req of
        Read (Fd fd) ptr len -> do
            n <- runOp $ URing.RecvPtrOp (Fd fd) ptr (fromIntegral len) 0
            return (fromIntegral n)
            
        Write (Fd fd) ptr len -> do
            n <- runOp $ URing.SendPtrOp (Fd fd) (castPtr ptr) (fromIntegral len) 0
            return (fromIntegral n)
            
        Connect (Fd fd) addr -> do
             -- Use synchronous connect via Network.Socket as io_uring connect is not exposed
             fd' <- dup (Fd fd)
             sock <- mkSocket fd'
             Net.connect sock addr
             -- The socket wrapper 'sock' will be GC'd and close fd'. Original 'fd' remains.
            
        Close (Fd fd) -> do
            _ <- runOp $ URing.CloseOp (Fd fd)
            return ()
            
        -- Fallbacks or errors for others
        _ -> error $ "Request not supported in URing backend: " ++ show req

-- Helper (simplistic error)
errnoToIOError :: String -> URing.Errno -> Maybe Handle -> Maybe String -> IOError
errnoToIOError loc (URing.Errno e) _ _ = 
    userError $ loc ++ " failed with errno: " ++ show e

mkSocket :: Fd -> IO Net.Socket
mkSocket fd = Net.mkSocket (fromIntegral fd)
