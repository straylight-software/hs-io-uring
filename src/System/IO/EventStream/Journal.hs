{-# LANGUAGE RecordWildCards #-}

module System.IO.EventStream.Journal
  ( FileJournal(..)
  , openJournal
  ) where

import System.IO 
  ( Handle
  , IOMode(AppendMode)
  , openBinaryFile
  , hFlush
  , hClose
  , hSeek
  , SeekMode(SeekFromEnd)
  , hIsEOF
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Binary (encode, decode)
import Data.Binary.Get (runGet, getWord32le)
import Data.Binary.Put (runPut, putWord32le, putLazyByteString)
import System.IO.EventStream 
  ( EventStream(append, next, flush, close)
  )
import Data.Word (Word32)

-- | A simple file-backed journal
data FileJournal = FileJournal
  { journalHandle :: Handle
  , journalPath   :: FilePath
  }

openJournal :: FilePath -> IOMode -> IO FileJournal
openJournal path mode = do
  h <- openBinaryFile path mode
  -- If appending, ensure we are at the end
  case mode of
    AppendMode -> hSeek h SeekFromEnd 0
    _          -> return ()
  return $ FileJournal h path

instance EventStream FileJournal where
  append (FileJournal h _) entry = do
    let payload = encode entry
        len     = fromIntegral (LBS.length payload) :: Word32
        frame   = runPut $ do
          putWord32le len
          putLazyByteString payload
    
    LBS.hPut h frame
    hFlush h

  next (FileJournal h _) = do
    eof <- hIsEOF h
    if eof 
      then return Nothing
      else do
        -- Read Length (4 bytes)
        lenBytes <- BS.hGet h 4
        if BS.length lenBytes < 4
           then return Nothing -- Unexpected EOF
           else do
             let len = runGet getWord32le (LBS.fromStrict lenBytes)
             -- Read Payload
             payload <- LBS.hGet h (fromIntegral len)
             if LBS.length payload < fromIntegral len
                then return Nothing -- Unexpected EOF or corruption
                else return (Just (decode payload))

  flush (FileJournal h _) = hFlush h
  
  close (FileJournal h _) = hClose h
