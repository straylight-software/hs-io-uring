{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                   // system // io // reactor
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module System.IO.Reactor
  ( Reactor (..)
  , OutputIntent (..)
  ) where

import Data.ByteString (ByteString)
import System.IO.EventStream (Entry)

-- ════════════════════════════════════════════════════════════════════════════
--                                                                   // intents
-- ════════════════════════════════════════════════════════════════════════════

-- | Abstract Output Intent.
--
-- This represents an effect the system *wants* to happen.
-- In Replay mode, these are discarded or verified against a golden log.
-- In Live mode, these are executed by the Runtime.
--
-- This type forces a separation between "Deciding what to do" and "Doing it".
data OutputIntent
  = SendPacket !ByteString
  -- ^ Send raw bytes over the active network connection.
  | WriteFile !FilePath !ByteString
  -- ^ Write bytes to a local file (e.g. state snapshot).
  | LogMessage !String
  -- ^ Emit a structured log message.
  | QueryLLM !String
  -- ^ Request a completion from the LLM subsystem.
  deriving (Show, Eq)

-- ════════════════════════════════════════════════════════════════════════════
--                                                                 // interface
-- ════════════════════════════════════════════════════════════════════════════

-- | The Reactor Interface.
--
-- * s: The State type (e.g. ChatState)
-- * e: The Event payload type (e.g. ChatEvent)
--
-- A Reactor is a pure state machine. It must not perform I/O.
class Reactor s e | s -> e where
  -- | The initial state of the reactor.
  -- This is the state at T=0, before any events are processed.
  initialState :: s

  -- | The pure transition function.
  --
  -- Takes current state and an input entry, returns new state and intents.
  --
  -- > react :: State -> Entry Event -> (State, [Intent])
  react :: s -> Entry e -> (s, [OutputIntent])

  -- | Serialize state for snapshots.
  -- This enables "Save Game" functionality and faster replay catch-up.
  snapshot :: s -> ByteString
