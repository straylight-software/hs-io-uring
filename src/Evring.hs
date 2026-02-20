{-# OPTIONS_GHC -Wno-missing-import-lists #-}
-- | libevring: Pure state machine abstraction over io_uring.
--
-- libevring models async I/O as pure state machines (Mealy machines).
-- The core pattern is:
--
-- @
--   State × Event → State × [Operation]
-- @
--
-- Key abstractions:
--
-- * 'Machine': pure functional core with @initial@, @step@, @done@
-- * 'GeneratorMachine': extension for bulk operations (@wantsToSubmit@, @generate@)
-- * 'Trace': recorded events for deterministic replay testing
-- * 'run' / 'replay': execute with actual I/O or replay from trace
--
-- The separation between pure machines and the effectful runner enables:
--
-- 1. **Deterministic testing**: Replay traces without actual I/O
-- 2. **Property-based testing**: Generate random event sequences
-- 3. **Golden tests**: Compare traces across versions
-- 4. **Debugging**: Reproduce exact failure scenarios
--
-- = Quick Start
--
-- @
-- -- Define a machine
-- data MyMachine = MyMachine
--
-- instance Machine MyMachine where
--   type State MyMachine = MyState
--   initial _ = initialState
--   step _ s e = StepResult newState operations
--   done _ s = isComplete s
--
-- -- Run with actual I/O
-- result <- run defaultRingConfig MyMachine
--
-- -- Or run and record a trace
-- (result, trace) <- runTraced defaultRingConfig MyMachine
--
-- -- Replay the trace (no I/O, deterministic)
-- let replayResult = replay MyMachine (traceEvents trace)
-- @
--
-- = Reset-on-Ambiguity
--
-- For parsing upstream data (SSE, JSON, tool calls), ambiguity MUST reset
-- the connection. This is enforced by SIGIL machines which implement
-- strict parsing with the 'StrictParseResult' type:
--
-- @
-- data StrictParseResult a
--   = Ok a
--   | Incomplete
--   | Ambiguous  -- triggers connection reset
-- @
module Evring
  ( -- * Machine abstraction
    module Evring.Machine
    -- * Events and Operations
  , module Evring.Event
    -- * Resource handles
  , module Evring.Handle
    -- * Ring runner
  , module Evring.Ring
    -- * Trace recording
  , module Evring.Trace
  ) where

import Evring.Machine
import Evring.Event
import Evring.Handle
import Evring.Ring
import Evring.Trace
