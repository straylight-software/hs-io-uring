{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module System.IO.Reactor
  ( Reactor(..)
  , OutputIntent(..)
  ) where

import System.IO.EventStream (Entry)
import Data.ByteString (ByteString)

-- | Abstract Output Intent
-- This represents an effect the system *wants* to happen.
-- In Replay mode, these are discarded/verified.
-- In Live mode, these are executed.
data OutputIntent
  = SendPacket !ByteString
  | WriteFile !FilePath !ByteString
  | LogMessage !String
  deriving (Show, Eq)

-- | The Reactor Interface
-- s: State
-- e: Event payload type
class Reactor s e | s -> e where
  -- | The initial state of the reactor
  initialState :: s
  
  -- | The pure transition function
  -- Takes current state and an input entry, returns new state and intents
  react :: s -> Entry e -> (s, [OutputIntent])
  
  -- | Serialize state for snapshots (Optional but recommended)
  snapshot :: s -> ByteString
