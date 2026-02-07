module System.IoUring.Options
  ( Options(..)
  , optionsParser
  , withOptions
  ) where

import Options.Applicative (
    Parser, 
    switch, 
    long, 
    short, 
    help, 
    execParser, 
    info, 
    fullDesc, 
    progDesc, 
    header, 
    helper, 
    (<**>)
  )
import System.IoUring.Logging (withLogging, Severity(DebugS, InfoS), LogEnv, Namespace)

data Options = Options
  { optVerbose :: Bool
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> switch
      ( long "verbose"
     <> short 'v'
     <> help "Enable verbose logging (Debug level)" )

withOptions :: Namespace -> (Options -> LogEnv -> IO a) -> IO a
withOptions ns act = do
  opts <- execParser optsInfo
  let sev = if optVerbose opts then DebugS else InfoS
  withLogging ns sev (act opts)
  where
    optsInfo = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "io-uring application"
     <> header "io-uring - high performance I/O" )
