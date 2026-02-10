{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                // system // io // trinity // posix // tls
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--     TLS wrapper for Trinity Posix I/O. Provides secure TCP connections
--     using the 'tls' library with system certificate store.
--
--     The API mirrors Trinity.Posix but adds TLS handshake and encryption.
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module System.IO.Trinity.Posix.TLS
  ( -- * TLS Client
    TlsConnection (..)
  , tlsConnect
  , tlsSend
  , tlsRecv
  , tlsClose

    -- * Errors
  , TlsError (..)
  ) where

import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS (null)
import Data.ByteString.Lazy qualified as LBS
import Data.Default.Class (def)
import Data.Text (Text)
import Data.Text qualified as T (pack)
import Network.Socket
  ( AddrInfo (addrAddress, addrSocketType)
  , HostName
  , ServiceName
  , SocketType (Stream)
  , connect
  , defaultHints
  , getAddrInfo
  , openSocket
  )
import Network.TLS qualified as TLS
import Network.TLS.Extra.Cipher qualified as TLS (ciphersuite_strong)
import System.X509 (getSystemCertificateStore)

-- ════════════════════════════════════════════════════════════════════════════
--                                                                   // errors
-- ════════════════════════════════════════════════════════════════════════════

data TlsError
  = TlsConnectionFailed Text
  | TlsHandshakeFailed Text
  | TlsSendFailed Text
  | TlsRecvFailed Text
  | TlsConnectionClosed
  deriving (Show, Eq)

-- ════════════════════════════════════════════════════════════════════════════
--                                                               // tls client
-- ════════════════════════════════════════════════════════════════════════════

-- | A TLS connection wrapping a socket with encryption context.
data TlsConnection = TlsConnection
  { tlsContext :: TLS.Context
  }

-- | Connect to a TLS server (e.g. HTTPS on port 443).
tlsConnect :: HostName -> ServiceName -> IO (Either TlsError TlsConnection)
tlsConnect host port = do
  certStore <- getSystemCertificateStore
  let clientParams = mkClientParams host certStore

  result <- try $ do
    -- resolve and connect TCP
    let hints = defaultHints { addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
    sock <- openSocket addr
    connect sock (addrAddress addr)

    -- TLS handshake
    ctx <- TLS.contextNew sock clientParams
    TLS.handshake ctx
    pure $ TlsConnection ctx

  pure $ case result of
    Left (e :: SomeException) -> Left $ TlsConnectionFailed $ T.pack $ show e
    Right conn -> Right conn

  where
    mkClientParams hostname certStore =
      (TLS.defaultParamsClient hostname "")
        { TLS.clientSupported = def
            { TLS.supportedCiphers = TLS.ciphersuite_strong
            }
        , TLS.clientShared = def
            { TLS.sharedCAStore = certStore
            }
        }

-- | Send data over a TLS connection.
tlsSend :: TlsConnection -> ByteString -> IO (Either TlsError ())
tlsSend (TlsConnection ctx) bs = do
  result <- try $ TLS.sendData ctx (LBS.fromStrict bs)
  pure $ case result of
    Left (e :: SomeException) -> Left $ TlsSendFailed $ T.pack $ show e
    Right () -> Right ()

-- | Receive data from a TLS connection.
--
-- Note: The 'maxBytes' parameter is a hint; TLS may return less or more
-- data depending on the underlying record boundaries.
tlsRecv :: TlsConnection -> Int -> IO (Either TlsError ByteString)
tlsRecv (TlsConnection ctx) _maxBytes = do
  result <- try $ TLS.recvData ctx
  pure $ case result of
    Left (e :: SomeException) -> Left $ TlsRecvFailed $ T.pack $ show e
    Right bs
      | BS.null bs -> Left TlsConnectionClosed
      | otherwise -> Right bs

-- | Close a TLS connection gracefully.
tlsClose :: TlsConnection -> IO ()
tlsClose (TlsConnection ctx) = do
  _ <- try $ TLS.bye ctx :: IO (Either SomeException ())
  TLS.contextClose ctx
