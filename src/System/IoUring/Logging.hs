{-# LANGUAGE OverloadedStrings #-}

module System.IoUring.Logging
  ( withLogging
  , LogEnv
  , Namespace
  , Severity(..)
  , runKatipContextT
  , KatipContextT
  , logMsg
  , sl
  ) where

import Control.Exception (bracket)
import Katip (
    mkHandleScribe, 
    ColorStrategy(ColorIfTerminal), 
    permitItem, 
    Verbosity(V2), 
    registerScribe, 
    defaultScribeSettings, 
    initLogEnv, 
    closeScribes, 
    LogEnv, 
    Namespace, 
    Severity(DebugS, InfoS, NoticeS, WarningS, ErrorS, CriticalS, AlertS, EmergencyS), 
    runKatipContextT, 
    KatipContextT, 
    logMsg, 
    sl
  )
import System.IO (stdout)

-- | Initialize Katip logging with a simple stdout scribe
withLogging :: Namespace -> Severity -> (LogEnv -> IO a) -> IO a
withLogging ns minSev action = do
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem minSev) V2
  let mkLogEnv = do
        le <- initLogEnv ns "production"
        registerScribe "stdout" handleScribe defaultScribeSettings le
        
  bracket mkLogEnv closeScribes action
