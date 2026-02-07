{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module System.IoUring.Reactor
  ( Reactor
  , withReactor
  , submitRequest
  ) where

import Control.Concurrent (forkIO, killThread, ThreadId, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar, tryTakeMVar, modifyMVar, modifyMVar_, withMVar)
import Control.Exception (finally)
import Control.Monad (forever, when)
import Data.IORef (newIORef, readIORef, modifyIORef)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Foreign (Ptr, castPtr, nullPtr)
import Data.Primitive (mutablePrimArrayContents, primArrayContents)
import System.Posix.Types (Fd(..))
import System.IO (hPutStrLn, stderr)

import System.IoUring (

    IOCtx(..), 
    CapCtx(..), 
    IoOp(..), 
    IoResult(..), 
    BatchPrep, 
    ioCtxParams, 
    ioBatchSizeLimit, 
    Errno(..)
  )
import qualified System.IoUring.URing as URing
import System.IoUring.Internal.FFI (
    c_io_uring_get_sqe,
    c_hs_uring_prep_nop,
    c_hs_uring_prep_read,
    c_hs_uring_prep_write,
    c_hs_uring_prep_readv,
    c_hs_uring_prep_writev,
    c_hs_uring_prep_recv,
    c_hs_uring_prep_send,
    c_hs_uring_prep_send_zc,
    c_hs_uring_prep_accept,
    c_hs_uring_sqe_set_data,
    c_hs_uring_prep_poll_add,
    c_hs_uring_prep_poll_remove,
    c_hs_uring_prep_fsync,
    c_hs_uring_prep_timeout,
    c_hs_uring_prep_timeout_remove,
    c_hs_uring_prep_openat,
    c_hs_uring_prep_close,
    c_hs_uring_prep_fallocate,
    c_hs_uring_prep_splice,
    c_hs_uring_prep_tee,
    c_hs_uring_prep_shutdown,
    c_hs_uring_prep_renameat,
    c_hs_uring_prep_unlinkat,
    c_hs_uring_prep_mkdirat,
    c_hs_uring_prep_symlinkat,
    c_hs_uring_prep_linkat,
    c_hs_uring_prep_madvise,
    c_hs_uring_prep_fadvise
  )

-- | A reactor manages async submissions and completions.
data Reactor = Reactor
  { rCtx       :: !IOCtx
  , rSlots     :: !(Vector (MVar IoResult))
  , rFreeSlots :: !(MVar [Int]) -- Simple free list
  , rThread    :: !(MVar ThreadId) -- The poller thread
  , rLock      :: !(MVar ()) -- Lock for submission safety
  }

withReactor :: IOCtx -> (Reactor -> IO a) -> IO a
withReactor ctx action = do
  let params = ioCtxParams ctx
      count = ioBatchSizeLimit params * 4 -- Allow more pending ops than batch size
  
  -- Create slots
  slots <- V.generateM count $ \_ -> newEmptyMVar
  
  -- Create free list
  freeList <- newMVar [0 .. count - 1]
  
  threadMVar <- newEmptyMVar
  lockMVar <- newMVar ()
  
  let reactor = Reactor
        { rCtx = ctx
        , rSlots = slots
        , rFreeSlots = freeList
        , rThread = threadMVar
        , rLock = lockMVar
        }
        
  -- Start poller thread
  tid <- forkIO $ runPoller reactor
  putMVar threadMVar tid
  
  finally (action reactor) (killThread tid)

runPoller :: Reactor -> IO ()
runPoller Reactor{..} = forever $ do
  -- Debug
  -- hPutStrLn stderr "Poller loop..."
  
  -- We assume one ring for simplicity in this reactor
  let (IOCtx caps) = rCtx
  if V.null caps 
    then return () 
    else do
      let cap = V.head caps
          uring = _capURing cap
      
      -- Use peekIO to avoid blocking issues in some environments
      mComp <- URing.peekIO uring
      case mComp of
        Nothing -> threadDelay 1 -- Yield/short sleep
        Just comp -> do
          let idx = fromIntegral $ case URing.completionId comp of URing.IOOpId w -> w
              res = case URing.completionRes comp of
                      URing.IOResult r -> 
                         if r < 0 
                           then IoErrno (Errno (fromIntegral (-r)))
                           else Complete (fromIntegral r)
          
          -- Find slot and fill
          if idx >= 0 && idx < V.length rSlots then do
             let slotMVar = rSlots V.! idx
             putMVar slotMVar res
          else hPutStrLn stderr $ "Invalid slot index: " ++ show idx

submitRequest :: Reactor -> BatchPrep -> IO IoResult
submitRequest Reactor{..} prep = do
  -- Alloc slot
  slotIdx <- modifyMVar rFreeSlots $ \list -> case list of
    [] -> fail "Reactor: Out of slots"
    (x:xs) -> return (xs, x)
    
  let slotMVar = rSlots V.! slotIdx
  
  -- Ensure MVar is empty (should be)
  _ <- tryTakeMVar slotMVar 
  
  submitAsyncToRing slotIdx
  
  -- Wait for result
  res <- takeMVar slotMVar
  
  -- Free slot
  modifyMVar_ rFreeSlots $ \list -> return (slotIdx:list)
  
  return res

  where
    submitAsyncToRing :: Int -> IO ()
    submitAsyncToRing slotIdx = withMVar rLock $ \_ -> do
       let (IOCtx caps) = rCtx
           cap = V.head caps
           uring = _capURing cap
           ringPtr = URing.uRingPtr uring
       
       -- Collect ops (expecting 1)
       ref <- newIORef []
       prep (\op -> modifyIORef ref (op:))
       ops <- readIORef ref
       
       case ops of
         [op] -> do
             prepareOp ringPtr slotIdx op
             res <- URing.submitIO uring
             when (res < 0) $ fail $ "Reactor: submitIO failed: " ++ show res
         _ -> fail "Reactor: submitRequest supports exactly one op"

prepareOp :: Ptr () -> Int -> IoOp -> IO ()
prepareOp ringPtr idx op = do
    sqe <- c_io_uring_get_sqe ringPtr
    when (sqe == nullPtr) $ fail "SQ ring full"
    
    let userData = fromIntegral idx
    
    case op of
          NopOp -> c_hs_uring_prep_nop sqe
          
          ReadOp (Fd fd) off buf _ len -> do
            let ptr = mutablePrimArrayContents buf
            c_hs_uring_prep_read sqe fd (castPtr ptr) (fromIntegral len) (fromIntegral off)
            
          ReadPtrOp (Fd fd) off ptr len -> do
            c_hs_uring_prep_read sqe fd (castPtr ptr) (fromIntegral len) (fromIntegral off)
            
          WriteOp (Fd fd) off buf _ len -> do
            let ptr = primArrayContents buf
            c_hs_uring_prep_write sqe fd (castPtr ptr) (fromIntegral len) (fromIntegral off)

          WritePtrOp (Fd fd) off ptr len -> do
            c_hs_uring_prep_write sqe fd (castPtr ptr) (fromIntegral len) (fromIntegral off)

          ReadvOp (Fd fd) off iovs cnt -> do
            c_hs_uring_prep_readv sqe fd iovs (fromIntegral cnt) (fromIntegral off)
            
          WritevOp (Fd fd) off iovs cnt -> do
            c_hs_uring_prep_writev sqe fd iovs (fromIntegral cnt) (fromIntegral off)
            
          RecvOp (Fd fd) buf _ len flags -> do
             let ptr = mutablePrimArrayContents buf
             c_hs_uring_prep_recv sqe fd (castPtr ptr) len (fromIntegral flags)

          RecvPtrOp (Fd fd) ptr len flags -> do
             c_hs_uring_prep_recv sqe fd (castPtr ptr) len (fromIntegral flags)

          SendOp (Fd fd) buf _ len flags -> do
             let ptr = primArrayContents buf
             c_hs_uring_prep_send sqe fd (castPtr ptr) len (fromIntegral flags)

          SendPtrOp (Fd fd) ptr len flags -> do
             c_hs_uring_prep_send sqe fd (castPtr ptr) len (fromIntegral flags)

          SendZcOp (Fd fd) buf _ len flags zcFlags -> do
             let ptr = primArrayContents buf
             c_hs_uring_prep_send_zc sqe fd (castPtr ptr) len (fromIntegral flags) (fromIntegral zcFlags)

          SendZcPtrOp (Fd fd) ptr len flags zcFlags -> do
             c_hs_uring_prep_send_zc sqe fd (castPtr ptr) len (fromIntegral flags) (fromIntegral zcFlags)

          AcceptOp (Fd fd) flags addrPtr lenPtr -> do
             c_hs_uring_prep_accept sqe fd (castPtr addrPtr) (castPtr lenPtr) (fromIntegral flags)

          PollAddOp (Fd fd) mask -> 
             c_hs_uring_prep_poll_add sqe fd (fromIntegral mask)
             
          PollRemoveOp targetUserData ->
             c_hs_uring_prep_poll_remove sqe (fromIntegral targetUserData)
             
          FsyncOp (Fd fd) flags ->
             c_hs_uring_prep_fsync sqe fd (fromIntegral flags)
             
          TimeoutOp ts count flags ->
             c_hs_uring_prep_timeout sqe ts (fromIntegral count) (fromIntegral flags)
             
          TimeoutRemoveOp targetUserData flags ->
             c_hs_uring_prep_timeout_remove sqe (fromIntegral targetUserData) (fromIntegral flags)
             
          OpenatOp (Fd dfd) path flags mode ->
             c_hs_uring_prep_openat sqe dfd path (fromIntegral flags) (fromIntegral mode)
             
          CloseOp (Fd fd) ->
             c_hs_uring_prep_close sqe fd
             
          FallocateOp (Fd fd) mode off len ->
             c_hs_uring_prep_fallocate sqe fd (fromIntegral mode) off len
             
          SpliceOp (Fd fd_in) off_in (Fd fd_out) off_out nbytes flags ->
             c_hs_uring_prep_splice sqe fd_in off_in fd_out off_out (fromIntegral nbytes) (fromIntegral flags)
             
          TeeOp (Fd fd_in) (Fd fd_out) nbytes flags ->
             c_hs_uring_prep_tee sqe fd_in fd_out (fromIntegral nbytes) (fromIntegral flags)
             
          ShutdownOp (Fd fd) how ->
             c_hs_uring_prep_shutdown sqe fd (fromIntegral how)
             
          RenameatOp (Fd olddfd) oldpath (Fd newdfd) newpath flags ->
             c_hs_uring_prep_renameat sqe olddfd oldpath newdfd newpath (fromIntegral flags)
             
          UnlinkatOp (Fd dfd) path flags ->
             c_hs_uring_prep_unlinkat sqe dfd path (fromIntegral flags)
             
          MkdiratOp (Fd dfd) path mode ->
             c_hs_uring_prep_mkdirat sqe dfd path (fromIntegral mode)
             
          SymlinkatOp target (Fd newdfd) linkpath ->
             c_hs_uring_prep_symlinkat sqe target newdfd linkpath
             
          LinkatOp (Fd olddfd) oldpath (Fd newdfd) newpath flags ->
             c_hs_uring_prep_linkat sqe olddfd oldpath newdfd newpath (fromIntegral flags)
             
          MadviseOp addr len advice ->
             c_hs_uring_prep_madvise sqe (castPtr addr) len (fromIntegral advice)
             
          FadviseOp (Fd fd) off len advice ->
             c_hs_uring_prep_fadvise sqe fd off len (fromIntegral advice)
          
          _ -> return ()
          
    c_hs_uring_sqe_set_data sqe userData
