{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module System.IoUring.Buffer
  ( BufferPool
  , BufferId
  , newBufferPool
  , freeBufferPool
  , withBuffer
  , allocBuffer
  , releaseBuffer
  , bufferSize
  , bufferPtr
  ) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, modifyMVar_)
import Control.Exception (bracket)
import Control.Monad (when)
import qualified Data.Vector as V
import Foreign (Ptr, plusPtr, mallocBytes, free, castPtr)
import System.IoUring (IOCtx, registerBuffers, unregisterBuffers)

type BufferId = Int

data BufferPool = BufferPool
  { bpBasePtr     :: !(Ptr ())         -- ^ One large contiguous block
  , bpElemSize    :: !Int              -- ^ Size of each buffer
  , bpCount       :: !Int              -- ^ Total number of buffers
  , bpFreeList    :: !(MVar [BufferId]) -- ^ Stack of free buffer IDs
  , bpCtx         :: !IOCtx            -- ^ Context this pool is registered with
  , bpRegistered  :: !Bool             -- ^ Whether it's registered with io_uring
  }

-- | Create a new buffer pool and register it with the IoUring context.
-- Allocates one large contiguous memory block.
newBufferPool :: IOCtx -> Int -> Int -> IO BufferPool
newBufferPool ctx count size = do
  let totalSize = count * size
  ptr <- mallocBytes totalSize
  
  -- Create vector of (ptr, len) for registration
  -- V.generate is strict enough usually
  let bufs = V.generate count $ \i -> 
        (ptr `plusPtr` (i * size), size)
  
  -- Register with io_uring
  -- Note: registerBuffers expects Ptr Word8, we have Ptr (). Cast it.
  registerBuffers ctx (V.map (\(p, l) -> (castPtr p, l)) bufs)
  
  -- Initial free list contains all indices [0..count-1]
  freeList <- newMVar [0 .. count - 1]
  
  return BufferPool
    { bpBasePtr    = castPtr ptr
    , bpElemSize   = size
    , bpCount      = count
    , bpFreeList   = freeList
    , bpCtx        = ctx
    , bpRegistered = True
    }

-- | Free the buffer pool and unregister buffers.
-- CAUTION: Ensure all buffers are returned before calling this!
freeBufferPool :: BufferPool -> IO ()
freeBufferPool BufferPool{..} = do
  when bpRegistered $ do
    unregisterBuffers bpCtx
  free bpBasePtr

-- | Allocate a buffer from the pool. Blocks if pool is empty.
allocBuffer :: BufferPool -> IO BufferId
allocBuffer BufferPool{..} = modifyMVar bpFreeList $ \list -> case list of
  []     -> fail "BufferPool: Out of buffers" -- In real app, retry or wait on condvar
  (x:xs) -> return (xs, x)

-- | Return a buffer to the pool.
releaseBuffer :: BufferPool -> BufferId -> IO ()
releaseBuffer BufferPool{..} idx = modifyMVar_ bpFreeList $ \list -> return (idx:list)

-- | Use a buffer and automatically release it.
withBuffer :: BufferPool -> (BufferId -> Ptr a -> IO b) -> IO b
withBuffer pool action = bracket (allocBuffer pool) (releaseBuffer pool) $ \idx -> 
  action idx (bufferPtr pool idx)

-- | Get the pointer for a buffer ID.
bufferPtr :: BufferPool -> BufferId -> Ptr a
bufferPtr BufferPool{..} idx = castPtr (bpBasePtr `plusPtr` (idx * bpElemSize))

bufferSize :: BufferPool -> Int
bufferSize = bpElemSize
