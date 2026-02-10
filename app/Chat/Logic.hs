{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                // chat // logic
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Chat.Logic
  ( ChatEvent (..)
  , ChatState (..)
  , Message (..)
  ) where

import Data.Binary (Binary)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Generics (Generic)
import System.IO.EventStream (Entry (event))
import System.IO.Reactor
  ( OutputIntent (LogMessage, QueryLLM, SendPacket)
  , Reactor (initialState, react, snapshot)
  )

-- ════════════════════════════════════════════════════════════════════════════
--                                                                    // domain
-- ════════════════════════════════════════════════════════════════════════════

-- | Domain Events.
-- These are the inputs to the system.
data ChatEvent
  = UserInput Text
  -- ^ User typed text.
  | NetReceived Text
  -- ^ Network received text.
  | NetError String
  -- ^ Network error.
  | AICompletion Text
  -- ^ LLM generated text.
  deriving (Show, Eq, Generic)

instance Binary ChatEvent

-- | Domain State.
-- This is the "Model" in MVU.
data ChatState = ChatState
  { messages :: [Message]
  , status :: Text
  }
  deriving (Show, Eq)

data Message = Message
  { msgRole :: Text
  , msgContent :: Text
  }
  deriving (Show, Eq)

-- ════════════════════════════════════════════════════════════════════════════
--                                                                   // reactor
-- ════════════════════════════════════════════════════════════════════════════

-- | Pure Logic Implementation.
--
-- This function is the heart of the application. It must be deterministic.
instance Reactor ChatState ChatEvent where
  initialState = ChatState
    { messages = []
    , status = "Ready"
    }

  react state entry = case event entry of
    UserInput text -> handleUserInput state text
    NetReceived text -> handleNetReceived state text
    NetError err -> handleNetError state err
    AICompletion text -> handleAICompletion state text

  snapshot _ = "" -- TODO: Implement serialization

-- ════════════════════════════════════════════════════════════════════════════
--                                                                  // handlers
-- ════════════════════════════════════════════════════════════════════════════

handleUserInput :: ChatState -> Text -> (ChatState, [OutputIntent])
handleUserInput state text =
  let newMsg = Message "user" text
      newState = state { messages = newMsg : messages state, status = "Thinking..." }
      -- Generate intent to send packet AND query LLM
      intents = [SendPacket (T.encodeUtf8 text), QueryLLM (T.unpack text)]
  in (newState, intents)

handleNetReceived :: ChatState -> Text -> (ChatState, [OutputIntent])
handleNetReceived state text =
  let newMsg = Message "ai" text
      newState = state { messages = newMsg : messages state, status = "Ready" }
  in (newState, [])

handleNetError :: ChatState -> String -> (ChatState, [OutputIntent])
handleNetError state err =
  let newState = state { status = T.pack ("Error: " ++ err) }
  in (newState, [LogMessage ("Network Error: " ++ err)])

handleAICompletion :: ChatState -> Text -> (ChatState, [OutputIntent])
handleAICompletion state text =
  let newMsg = Message "ai" text
      newState = state { messages = newMsg : messages state, status = "Ready" }
  in (newState, [])
