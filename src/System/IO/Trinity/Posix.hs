{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                // system // io // trinity // posix
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--     High-level Posix I/O for Trinity. This is the "simple" backend that
--     uses GHC's IO manager (epoll/kqueue under the hood). It provides a
--     clean ByteString-based interface that can be swapped for io_uring later.
--
--     The API is designed to match Trinity's intent model:
--       - Operations return Either for explicit error handling
--       - No exceptions escape (caught and wrapped)
--       - Timeout support built-in
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module System.IO.Trinity.Posix
  ( -- * TCP Client
    TcpConnection (..)
  , tcpConnect
  , tcpSend
  , tcpRecv
  , tcpClose

    -- * TCP Server
  , TcpListener (..)
  , tcpListen
  , tcpAccept
  , tcpCloseListener

    -- * File I/O
  , FileHandle (..)
  , IOMode (..)
  , fileOpen
  , fileRead
  , fileWrite
  , fileClose
  , fileAppend

    -- * Errors
  , PosixError (..)
  ) where

import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS (hGet, hPut, null)
import Data.Text (Text)
import Data.Text qualified as T (pack)
import Network.Socket
  ( AddrInfo (addrAddress, addrFamily, addrSocketType)
  , Family (AF_INET)
  , HostName
  , ServiceName
  , SockAddr
  , Socket
  , SocketOption (ReuseAddr)
  , SocketType (Stream)
  , accept
  , bind
  , close
  , connect
  , defaultHints
  , getAddrInfo
  , gracefulClose
  , listen
  , openSocket
  , setSocketOption
  )
import Network.Socket.ByteString (recv, sendAll)
import System.IO
  ( Handle
  , IOMode (AppendMode, ReadMode, ReadWriteMode, WriteMode)
  , hClose
  , hFlush
  , openBinaryFile
  )

-- ════════════════════════════════════════════════════════════════════════════
--                                                                   // errors
-- ════════════════════════════════════════════════════════════════════════════

data PosixError
  = ConnectionFailed Text
  | ConnectionClosed
  | SendFailed Text
  | RecvFailed Text
  | BindFailed Text
  | AcceptFailed Text
  | FileOpenFailed Text
  | FileReadFailed Text
  | FileWriteFailed Text
  deriving (Show, Eq)

-- ════════════════════════════════════════════════════════════════════════════
--                                                               // tcp client
-- ════════════════════════════════════════════════════════════════════════════

newtype TcpConnection = TcpConnection { tcpSocket :: Socket }

-- | Connect to a TCP server.
tcpConnect :: HostName -> ServiceName -> IO (Either PosixError TcpConnection)
tcpConnect host port = do
  result <- try $ do
    let hints = defaultHints { addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
    sock <- openSocket addr
    connect sock (addrAddress addr)
    pure $ TcpConnection sock
  pure $ case result of
    Left (e :: SomeException) -> Left $ ConnectionFailed $ T.pack $ show e
    Right conn -> Right conn

-- | Send data over a TCP connection.
tcpSend :: TcpConnection -> ByteString -> IO (Either PosixError ())
tcpSend (TcpConnection sock) bs = do
  result <- try $ sendAll sock bs
  pure $ case result of
    Left (e :: SomeException) -> Left $ SendFailed $ T.pack $ show e
    Right () -> Right ()

-- | Receive data from a TCP connection.
tcpRecv :: TcpConnection -> Int -> IO (Either PosixError ByteString)
tcpRecv (TcpConnection sock) maxBytes = do
  result <- try $ recv sock maxBytes
  pure $ case result of
    Left (e :: SomeException) -> Left $ RecvFailed $ T.pack $ show e
    Right bs
      | BS.null bs -> Left ConnectionClosed
      | otherwise -> Right bs

-- | Close a TCP connection.
tcpClose :: TcpConnection -> IO ()
tcpClose (TcpConnection sock) = gracefulClose sock 5000

-- ════════════════════════════════════════════════════════════════════════════
--                                                               // tcp server
-- ════════════════════════════════════════════════════════════════════════════

newtype TcpListener = TcpListener { listenerSocket :: Socket }

-- | Create a TCP listener.
tcpListen :: HostName -> ServiceName -> IO (Either PosixError TcpListener)
tcpListen host port = do
  result <- try $ do
    let hints = defaultHints { addrSocketType = Stream, addrFamily = AF_INET }
    addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
    sock <- openSocket addr
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 128
    pure $ TcpListener sock
  pure $ case result of
    Left (e :: SomeException) -> Left $ BindFailed $ T.pack $ show e
    Right listener -> Right listener

-- | Accept a connection on a listener.
tcpAccept :: TcpListener -> IO (Either PosixError (TcpConnection, SockAddr))
tcpAccept (TcpListener sock) = do
  result <- try $ accept sock
  pure $ case result of
    Left (e :: SomeException) -> Left $ AcceptFailed $ T.pack $ show e
    Right (clientSock, addr) -> Right (TcpConnection clientSock, addr)

-- | Close a TCP listener.
tcpCloseListener :: TcpListener -> IO ()
tcpCloseListener (TcpListener sock) = close sock

-- ════════════════════════════════════════════════════════════════════════════
--                                                                 // file i/o
-- ════════════════════════════════════════════════════════════════════════════

newtype FileHandle = FileHandle { fileH :: Handle }

-- | Open a file for reading, writing, or both.
fileOpen :: FilePath -> IOMode -> IO (Either PosixError FileHandle)
fileOpen path mode = do
  result <- try $ openBinaryFile path mode
  pure $ case result of
    Left (e :: SomeException) -> Left $ FileOpenFailed $ T.pack $ show e
    Right h -> Right $ FileHandle h

-- | Read from a file.
fileRead :: FileHandle -> Int -> IO (Either PosixError ByteString)
fileRead (FileHandle h) maxBytes = do
  result <- try $ BS.hGet h maxBytes
  pure $ case result of
    Left (e :: SomeException) -> Left $ FileReadFailed $ T.pack $ show e
    Right bs -> Right bs

-- | Write to a file.
fileWrite :: FileHandle -> ByteString -> IO (Either PosixError ())
fileWrite (FileHandle h) bs = do
  result <- try $ do
    BS.hPut h bs
    hFlush h
  pure $ case result of
    Left (e :: SomeException) -> Left $ FileWriteFailed $ T.pack $ show e
    Right () -> Right ()

-- | Append to a file (convenience wrapper).
fileAppend :: FilePath -> ByteString -> IO (Either PosixError ())
fileAppend path bs = do
  result <- fileOpen path AppendMode
  case result of
    Left e -> pure $ Left e
    Right fh -> do
      writeResult <- fileWrite fh bs
      fileClose fh
      pure writeResult

-- | Close a file.
fileClose :: FileHandle -> IO ()
fileClose (FileHandle h) = hClose h
