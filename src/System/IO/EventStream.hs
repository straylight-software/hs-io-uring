{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                    // system // io // stream
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module System.IO.EventStream
  ( -- * Core Types
    Entry (..)
  , EventStream (..)
  , StreamMode (..)

    -- * Serialization
  , encodeEntry
  , decodeEntry
  ) where

import Data.Binary (Binary, decode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)

-- ════════════════════════════════════════════════════════════════════════════
--                                                                // core types
-- ════════════════════════════════════════════════════════════════════════════

-- | The fundamental atom of the system.
--
-- An Entry represents a discrete, immutable event in the timeline.
-- This structure is the basis of the "Replay-First" architecture.
data Entry a = Entry
  { sequenceId :: !Word64
  -- ^ Strictly increasing monotonic tick.
  -- Used to detect gaps or reordering in the log.
  , timestamp :: !Word64
  -- ^ Physical time (nanoseconds since epoch).
  -- Note: Logic should use this timestamp, not 'getCurrentTime', to ensure
  -- determinism during replay.
  , checksum :: !Word32
  -- ^ CRC32 or Adler32 of the payload.
  -- Ensures data integrity on disk.
  , event :: !a
  -- ^ The domain-specific event payload.
  }
  deriving (Show, Eq, Generic)

instance (Binary a) => Binary (Entry a)

-- | Operation mode for the EventStream runtime.
data StreamMode
  = Live
  -- ^ Normal operation: Poll I/O -> Write to Log -> React
  | Replay
  -- ^ Deterministic replay: Read from Log -> React -> Verify
  deriving (Show, Eq)

-- ════════════════════════════════════════════════════════════════════════════
--                                                                 // interface
-- ════════════════════════════════════════════════════════════════════════════

-- | The Interface for an EventStream backend.
--
-- This abstracts over the storage mechanism (File, Memory, RingBuffer).
-- In production, this is usually 'System.IO.EventStream.Journal'.
class EventStream s where
  -- | Append a new entry to the stream (Live Mode).
  -- This must be atomic and durable (flushed) before the runtime proceeds.
  append :: (Binary a) => s -> Entry a -> IO ()

  -- | Read the next entry from the stream (Replay Mode).
  -- Returns 'Nothing' if the stream is exhausted (EOF).
  next :: (Binary a) => s -> IO (Maybe (Entry a))

  -- | Flush any buffered writes to persistence.
  flush :: s -> IO ()

  -- | Close the stream and release resources.
  close :: s -> IO ()

-- ════════════════════════════════════════════════════════════════════════════
--                                                             // serialization
-- ════════════════════════════════════════════════════════════════════════════

-- | Helper to encode an entry to lazy bytestring.
encodeEntry :: (Binary a) => Entry a -> LBS.ByteString
encodeEntry = encode

-- | Helper to decode an entry from lazy bytestring.
decodeEntry :: (Binary a) => LBS.ByteString -> Entry a
decodeEntry = decode
