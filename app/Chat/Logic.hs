{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Chat.Logic
  ( ChatEvent(..)
  , ChatState(..)
  , Message(..)
  ) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.IO.Reactor (Reactor(react, initialState, snapshot), OutputIntent(SendPacket, LogMessage, QueryLLM))
import System.IO.EventStream (Entry(event))

-- | Domain Events
data ChatEvent
  = UserInput Text        -- ^ User typed text
  | NetReceived Text      -- ^ Network received text
  | NetError String       -- ^ Network error
  | AICompletion Text     -- ^ LLM generated text
  deriving (Show, Eq, Generic)

instance Binary ChatEvent

-- | Domain State
data ChatState = ChatState
  { messages    :: [Message]
  , status      :: Text
  } deriving (Show, Eq)

data Message = Message
  { msgRole    :: Text
  , msgContent :: Text
  } deriving (Show, Eq)

-- | Pure Logic Implementation
instance Reactor ChatState ChatEvent where
  initialState = ChatState
    { messages = []
    , status = "Ready"
    }

  react state entry = case event entry of
    UserInput text ->
      let newMsg = Message "user" text
          newState = state { messages = newMsg : messages state, status = "Thinking..." }
          -- Generate intent to send packet AND query LLM
          intents = [SendPacket (T.encodeUtf8 text), QueryLLM (T.unpack text)]
      in (newState, intents)

    NetReceived text ->
      let newMsg = Message "ai" text
          newState = state { messages = newMsg : messages state, status = "Ready" }
      in (newState, [])

    NetError err ->
      let newState = state { status = T.pack ("Error: " ++ err) }
      in (newState, [LogMessage ("Network Error: " ++ err)])

    AICompletion text ->
      let newMsg = Message "ai" text
          newState = state { messages = newMsg : messages state, status = "Ready" }
      in (newState, [])

  snapshot _ = "" -- TODO: Implement serialization
