{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module System.IO.EventStream
  ( -- * Core Types
    Entry(..)
  , EventStream(..)
  , StreamMode(..)
  
    -- * Serialization
  , encodeEntry
  , decodeEntry
  ) where

import Data.Word (Word64, Word32)
import qualified Data.ByteString.Lazy as LBS
import Data.Binary (Binary, encode, decode)
import GHC.Generics (Generic)

-- | The fundamental atom of the system.
-- An Entry represents a discrete, immutable event in the timeline.
data Entry a = Entry
  { sequenceId :: !Word64          -- ^ Strictly increasing monotonic tick
  , timestamp  :: !Word64          -- ^ Physical time (nanoseconds since epoch)
  , checksum   :: !Word32          -- ^ CRC32 or Adler32 of the payload
  , event      :: !a               -- ^ The domain-specific event payload
  } deriving (Show, Eq, Generic)

instance Binary a => Binary (Entry a)

-- | Operation mode for the EventStream runtime
data StreamMode 
  = Live    -- ^ Normal operation: Poll I/O -> Write to Log -> React
  | Replay  -- ^ Deterministic replay: Read from Log -> React -> Verify
  deriving (Show, Eq)

-- | The Interface for an EventStream backend
-- This abstracts over the storage mechanism (File, Memory, RingBuffer)
class EventStream s where
  -- | Append a new entry to the stream (Live Mode)
  append :: Binary a => s -> Entry a -> IO ()
  
  -- | Read the next entry from the stream (Replay Mode)
  next :: Binary a => s -> IO (Maybe (Entry a))
  
  -- | Flush any buffered writes to persistence
  flush :: s -> IO ()
  
  -- | Close the stream
  close :: s -> IO ()

-- | Helper to encode an entry to lazy bytestring
encodeEntry :: Binary a => Entry a -> LBS.ByteString
encodeEntry = encode

-- | Helper to decode an entry from lazy bytestring
decodeEntry :: Binary a => LBS.ByteString -> Entry a
decodeEntry = decode
