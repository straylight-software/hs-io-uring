{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                   // system // io // journal
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module System.IO.EventStream.Journal
  ( FileJournal (..)
  , openJournal
  ) where

import Data.Binary (decode, encode)
import Data.Binary.Get (getWord32le, runGet)
import Data.Binary.Put (putLazyByteString, putWord32le, runPut)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Word (Word32)
import System.IO
  ( Handle
  , IOMode (AppendMode)
  , SeekMode (SeekFromEnd)
  , hClose
  , hFlush
  , hIsEOF
  , hSeek
  , openBinaryFile
  )
import System.IO.EventStream (EventStream (append, close, flush, next))

-- ════════════════════════════════════════════════════════════════════════════
--                                                                      // type
-- ════════════════════════════════════════════════════════════════════════════

-- | A simple file-backed journal.
--
-- This implementation uses a length-prefixed framing format:
-- [Length :: Word32LE] [Payload :: Bytes]
--
-- This ensures that we can detect partial writes or corruption.
data FileJournal = FileJournal
  { journalHandle :: Handle
  , journalPath :: FilePath
  }

-- ════════════════════════════════════════════════════════════════════════════
--                                                              // construction
-- ════════════════════════════════════════════════════════════════════════════

openJournal :: FilePath -> IOMode -> IO FileJournal
openJournal path ioMode = do
  handle <- openBinaryFile path ioMode
  -- if appending, ensure we are at the end
  seekToEndIfAppending handle ioMode
  pure $ FileJournal handle path
  where
    seekToEndIfAppending h AppendMode = hSeek h SeekFromEnd 0
    seekToEndIfAppending _ _ = pure ()

-- ════════════════════════════════════════════════════════════════════════════
--                                                             // implementation
-- ════════════════════════════════════════════════════════════════════════════

instance EventStream FileJournal where
  append (FileJournal h _) entry = do
    let
      payload = encode entry
      len = fromIntegral (LBS.length payload) :: Word32
      frame = runPut $ do
        putWord32le len
        putLazyByteString payload

    LBS.hPut h frame
    -- In a real high-perf system, we wouldn't flush every time,
    -- or we'd rely on the OS page cache + periodic fsync.
    -- For correctness/reproducibility safety, we flush.
    hFlush h

  next (FileJournal h _) = readNextEntry
    where
      readNextEntry = do
        eof <- hIsEOF h
        if eof then pure Nothing else readFramedEntry

      readFramedEntry = do
        lenBytes <- BS.hGet h 4  -- read length (4 bytes)
        readPayloadIfValid lenBytes

      readPayloadIfValid lenBytes
        | BS.length lenBytes < 4 = pure Nothing  -- unexpected eof
        | len <- runGet getWord32le (LBS.fromStrict lenBytes)
        = do
          payload <- LBS.hGet h (fromIntegral len)
          decodePayloadIfComplete len payload

      decodePayloadIfComplete len payload
        | LBS.length payload < fromIntegral len = pure Nothing  -- corruption
        | otherwise = pure (Just (decode payload))

  flush (FileJournal h _) = hFlush h

  close (FileJournal h _) = hClose h
