{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                              // chat // openai
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Chat.OpenAI
  ( OpenAIClient (..)
  , ChatMessage (..)
  , mkClient
  , mkOpenRouterClient
  , mkOpenAIClient
  , complete
  ) where

import Data.Aeson (FromJSON, ToJSON, decode, encode, object, (.:), (.=))
import Data.Aeson.Types (parseEither, withObject)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Generics (Generic)
import System.IO.Trinity.Posix.TLS (TlsConnection, tlsClose, tlsConnect, tlsRecv, tlsSend)

-- ════════════════════════════════════════════════════════════════════════════
--                                                                     // types
-- ════════════════════════════════════════════════════════════════════════════

data OpenAIClient = OpenAIClient
  { clientApiKey :: Text
  , clientModel :: Text
  , clientHost :: Text
  , clientPath :: Text
  }

data ChatMessage = ChatMessage
  { role :: Text
  , content :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON ChatMessage
instance FromJSON ChatMessage

-- ════════════════════════════════════════════════════════════════════════════
--                                                                // construction
-- ════════════════════════════════════════════════════════════════════════════

-- | Create client for OpenRouter (OpenAI-compatible API)
mkOpenRouterClient :: Text -> OpenAIClient
mkOpenRouterClient apiKey = OpenAIClient
  { clientApiKey = apiKey
  , clientModel = "anthropic/claude-sonnet-4"
  , clientHost = "openrouter.ai"
  , clientPath = "/api/v1/chat/completions"
  }

-- | Create client for OpenAI
mkOpenAIClient :: Text -> OpenAIClient
mkOpenAIClient apiKey = OpenAIClient
  { clientApiKey = apiKey
  , clientModel = "gpt-4o-mini"
  , clientHost = "api.openai.com"
  , clientPath = "/v1/chat/completions"
  }

-- | Default: tries OPENROUTER_API_KEY, falls back to OPENAI_API_KEY
mkClient :: Text -> OpenAIClient
mkClient = mkOpenRouterClient

-- ════════════════════════════════════════════════════════════════════════════
--                                                                  // completion
-- ════════════════════════════════════════════════════════════════════════════

complete :: OpenAIClient -> [ChatMessage] -> IO (Either Text Text)
complete client messages = do
  let host = T.unpack (clientHost client)

  connResult <- tlsConnect host "443"

  case connResult of
    Left err -> pure $ Left $ T.pack $ "Connection error: " ++ show err
    Right conn -> do
      -- build request
      let body = buildRequestBody client messages
          bodyLen = LBS.length body
          request = buildHttpRequest client bodyLen

      -- send request
      _ <- tlsSend conn request
      _ <- tlsSend conn (LBS.toStrict body)

      -- read response
      response <- readHttpResponse conn
      tlsClose conn
      pure $ parseResponse response

-- ════════════════════════════════════════════════════════════════════════════
--                                                                   // helpers
-- ════════════════════════════════════════════════════════════════════════════

buildRequestBody :: OpenAIClient -> [ChatMessage] -> LBS.ByteString
buildRequestBody client messages = encode $ object
  [ "model" .= clientModel client
  , "messages" .= messages
  , "max_tokens" .= (1024 :: Int)
  ]

buildHttpRequest :: OpenAIClient -> Int64 -> BS.ByteString
buildHttpRequest client contentLength = T.encodeUtf8 $ T.concat
  [ "POST " <> clientPath client <> " HTTP/1.1\r\n"
  , "Host: " <> clientHost client <> "\r\n"
  , "Authorization: Bearer " <> clientApiKey client <> "\r\n"
  , "Content-Type: application/json\r\n"
  , "Content-Length: " <> T.pack (show contentLength) <> "\r\n"
  , "Connection: close\r\n"
  , "\r\n"
  ]

readHttpResponse :: TlsConnection -> IO LBS.ByteString
readHttpResponse conn = readUntilDone LBS.empty
  where
    readUntilDone acc = do
      result <- tlsRecv conn 4096
      case result of
        Left _ -> pure acc  -- connection closed or error
        Right chunk
          | BS.null chunk -> pure acc
          | otherwise -> do
              let newAcc = acc <> LBS.fromStrict chunk
              -- check if we have complete response
              if hasCompleteResponse newAcc
                then pure newAcc
                else readUntilDone newAcc

    hasCompleteResponse bs
      | Just headerEnd <- findSubstring "\r\n\r\n" (LBS.toStrict bs)
      , headers <- BS.take headerEnd (LBS.toStrict bs)
      = if isChunkedEncoding headers
          then hasCompleteChunkedBody (LBS.drop (fromIntegral headerEnd + 4) bs)
          else case parseContentLength headers of
            Just contentLen ->
              let bodyLen = LBS.length bs - fromIntegral headerEnd - 4
              in bodyLen >= fromIntegral contentLen
            Nothing -> False
      | otherwise = False

    isChunkedEncoding headers =
      let headerLines = BS.split 10 headers
      in any (BS.isInfixOf "chunked") headerLines

    -- chunked body ends with "0\r\n\r\n"
    hasCompleteChunkedBody body = BS.isInfixOf "\r\n0\r\n" (LBS.toStrict body)

    findSubstring needle haystack = go 0
      where
        needleLen = BS.length needle
        haystackLen = BS.length haystack
        go i
          | i + needleLen > haystackLen = Nothing
          | BS.take needleLen (BS.drop i haystack) == needle = Just i
          | otherwise = go (i + 1)

    parseContentLength headers =
      let headerLines = BS.split 10 headers
          contentLengthLine = filter (BS.isPrefixOf "Content-Length:") headerLines
      in case contentLengthLine of
        [] -> Nothing
        (line:_) ->
          let valPart = BS.drop 15 line
              cleaned = BS.filter (\c -> c >= 48 && c <= 57) valPart
          in if BS.null cleaned
             then Nothing
             else Just (read (map (toEnum . fromEnum) (BS.unpack cleaned)) :: Int)

parseResponse :: LBS.ByteString -> Either Text Text
parseResponse raw
  | Just headerEnd <- findBodyStart (LBS.toStrict raw)
  , headers <- LBS.take (fromIntegral headerEnd - 4) raw
  , body <- LBS.drop (fromIntegral headerEnd) raw
  , decodedBody <- if isChunked headers then decodeChunked body else body
  = extractContent decodedBody
  | otherwise = Left "Failed to parse HTTP response"
  where
    isChunked h = BS.isInfixOf "chunked" (LBS.toStrict h)

    -- decode chunked transfer encoding
    decodeChunked body = decodeChunks (LBS.toStrict body) LBS.empty

    decodeChunks bs acc
      | BS.null bs = acc
      | Just (chunkSize, _) <- parseChunkSize bs
      , chunkSize == 0 = acc  -- final chunk
      | Just (chunkSize, rest) <- parseChunkSize bs
      , BS.length rest >= chunkSize + 2  -- +2 for trailing \r\n
      , chunkData <- BS.take chunkSize rest
      , remaining <- BS.drop (chunkSize + 2) rest
      = decodeChunks remaining (acc <> LBS.fromStrict chunkData)
      | otherwise = acc  -- malformed, return what we have

    parseChunkSize bs =
      let (sizeLine, rest) = BS.breakSubstring "\r\n" bs
          sizeStr = map (toEnum . fromEnum) (BS.unpack sizeLine)
          cleanSize = takeWhile (\c -> c `elem` ("0123456789abcdefABCDEF" :: String)) sizeStr
      in if null cleanSize || BS.length rest < 2
         then Nothing
         else Just (readHex cleanSize, BS.drop 2 rest)

    readHex :: String -> Int
    readHex s = case reads ("0x" ++ s) of
      [(n, "")] -> n
      _ -> 0

findBodyStart :: BS.ByteString -> Maybe Int
findBodyStart haystack = go 0
  where
    needle = "\r\n\r\n"
    needleLen = BS.length needle
    haystackLen = BS.length haystack
    go i
      | i + needleLen > haystackLen = Nothing
      | BS.take needleLen (BS.drop i haystack) == needle = Just (i + needleLen)
      | otherwise = go (i + 1)

extractContent :: LBS.ByteString -> Either Text Text
extractContent body = case decode body of
  Nothing -> Left $ "Failed to decode JSON: " <> T.decodeUtf8 (LBS.toStrict body)
  Just val -> case parseEither extractFromResponse val of
    Left err -> Left $ T.pack err
    Right responseContent -> Right responseContent
  where
    extractFromResponse = withObject "Response" $ \v -> do
      choices <- v .: "choices"
      case choices of
        [] -> fail "No choices in response"
        (choice:_) -> do
          msg <- choice .: "message"
          msg .: "content"
