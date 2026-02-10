{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                // app // main
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Main where

import Brick
  ( App (App, appAttrMap, appChooseCursor, appDraw, appHandleEvent, appStartEvent)
  , BrickEvent (AppEvent, VtyEvent)
  , EventM
  , Widget
  , customMain
  , get
  , halt
  , showFirstCursor
  )
import Brick.AttrMap qualified as A
import Brick.BChan qualified as BChan
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Core (padAll, padLeftRight, strWrap, txt, vBox)
import Brick.Widgets.Edit qualified as E
import Chat.Logic
  ( ChatEvent (AICompletion, NetError, NetReceived, UserInput)
  , ChatState (messages, status)
  , Message (Message)
  )
import Chat.OpenAI qualified as OpenAI
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform qualified as VCP
import Lens.Micro ((^.))
import Lens.Micro.Mtl (zoom, (.=))
import Lens.Micro.TH (makeLenses)
import System.IO.Trinity.Posix (TcpConnection, tcpConnect, tcpRecv, tcpSend, tcpClose)
import System.Environment (lookupEnv)
import System.IO (IOMode (ReadWriteMode))
import System.IO.EventStream
  ( Entry (Entry)
  , EventStream (append, next)
  , StreamMode (Live)
  )
import System.IO.EventStream.Journal (FileJournal, openJournal)
import System.IO.Reactor
  ( OutputIntent (LogMessage, QueryLLM, SendPacket)
  , Reactor (initialState, react)
  )
import System.IO.Trinity
  ( TrinityConfig (TrinityConfig, tMode, tStream, tTick)
  )

-- ════════════════════════════════════════════════════════════════════════════
--                                                                    // ui state
-- ════════════════════════════════════════════════════════════════════════════

-- | UI State (Ephemeral, Visual only).
-- The "Real" state is in the Reactor, but we need a view model for Brick.
data UIState = UIState
  { _uiMessages :: [Message]
  , _uiEditor :: E.Editor Text ()
  , _uiStatus :: Text
  }

makeLenses ''UIState

-- | Brick Event Wrapper.
data AppEvent
  = StateUpdate ChatState -- ^ Reactor pushed a new state.

-- ════════════════════════════════════════════════════════════════════════════
--                                                                        // view
-- ════════════════════════════════════════════════════════════════════════════

drawUI :: UIState -> [Widget ()]
drawUI st = [ui]
  where
    ui = C.center $ B.borderWithLabel (txt " Aleph Chat (Replay Architecture) ") $
      vBox
        [ msgList
        , B.hBorder
        , inputArea
        , B.hBorder
        , statusBar
        ]

    msgList = C.centerLayer $ vBox $ map drawMsg (reverse $ st ^. uiMessages)

    drawMsg (Message role content)
      | prefix <- if role == "user" then "You: " else "AI: "
      = padLeftRight 1 $ strWrap (prefix ++ T.unpack content)

    inputArea = padAll 1 $ E.renderEditor (txt . T.unlines) True (st ^. uiEditor)

    statusBar = padLeftRight 1 $ txt (st ^. uiStatus)

-- ════════════════════════════════════════════════════════════════════════════
--                                                                      // update
-- ════════════════════════════════════════════════════════════════════════════

appEvent
  :: BChan.BChan ChatEvent
  -> BChan.BChan AppEvent
  -> BrickEvent () AppEvent
  -> EventM () UIState ()
appEvent _ _ (AppEvent (StateUpdate newState)) = do
  uiMessages .= messages newState
  uiStatus .= status newState

appEvent reactorChan _ (VtyEvent (V.EvKey V.KEnter [])) = do
  st <- get
  let content = T.strip $ T.unlines $ E.getEditContents (st ^. uiEditor)
  if T.null content
    then pure ()
    else do
      liftIO $ BChan.writeBChan reactorChan (UserInput content)
      uiEditor .= E.editor () (Just 1) ""

appEvent _ _ (VtyEvent (V.EvKey V.KEsc [])) = halt

appEvent _ _ (VtyEvent e) = zoom uiEditor $ E.handleEditorEvent (VtyEvent e)

appEvent _ _ _ = pure ()

initialUI :: UIState
initialUI = UIState
  { _uiMessages = []
  , _uiEditor = E.editor () (Just 1) ""
  , _uiStatus = "Initializing..."
  }

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr []

app :: BChan.BChan ChatEvent -> BChan.BChan AppEvent -> App UIState AppEvent ()
app reactorChan uiChan = App
  { appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = appEvent reactorChan uiChan
  , appStartEvent = pure ()
  , appAttrMap = const theMap
  }

-- ════════════════════════════════════════════════════════════════════════════
--                                                                        // main
-- ════════════════════════════════════════════════════════════════════════════

main :: IO ()
main = do
  -- setup channels
  uiChan <- BChan.newBChan 10       -- reactor -> ui updates
  reactorChan <- BChan.newBChan 10  -- ui -> reactor inputs

  -- load openrouter client
  mApiKey <- lookupEnv "OPENROUTER_API_KEY"
  let openaiClient = OpenAI.mkClient . T.pack <$> mApiKey

  -- open journal
  journal <- openJournal "chat.journal" ReadWriteMode

  -- network setup (try to connect via Trinity.Posix)
  connResult <- tcpConnect "127.0.0.1" "8080"

  case connResult of
    Left _err ->
      -- no server, run offline
      runAppWithNetwork Nothing openaiClient uiChan reactorChan journal
    Right conn -> do
      -- connected, start input pump
      inputPumpId <- forkIO $ inputPump conn reactorChan
      runAppWithNetwork (Just conn) openaiClient uiChan reactorChan journal
      killThread inputPumpId
      tcpClose conn

  where
    inputPump conn reactorChan = inputLoop
      where
        inputLoop = do
          result <- tcpRecv conn 4096
          case result of
            Left _err -> do
              BChan.writeBChan reactorChan (NetError "Connection Closed")
              threadDelay 10_000_000
            Right bs -> do
              BChan.writeBChan reactorChan (NetReceived (T.decodeUtf8 bs))
              inputLoop

runAppWithNetwork
  :: Maybe TcpConnection
  -> Maybe OpenAI.OpenAIClient
  -> BChan.BChan AppEvent
  -> BChan.BChan ChatEvent
  -> FileJournal
  -> IO ()
runAppWithNetwork sock openaiClient uiChan reactorChan journal = do
  -- replay history
  putStrLn "Replaying journal..."
  initialStateReplayed <- replayLoop (initialState :: ChatState)
  putStrLn $ "Replay complete. Messages: " ++ show (length (messages initialStateReplayed))

  -- fork the runtime loop
  void $ forkIO $ runChatRuntime config uiChan journal initialStateReplayed executor

  -- start brick
  let initialUIReplayed = initialUI { _uiMessages = messages initialStateReplayed }
  vty <- VCP.mkVty V.defaultConfig
  void $ customMain vty (VCP.mkVty V.defaultConfig) (Just uiChan) (app reactorChan uiChan) initialUIReplayed

  where
    replayLoop state = do
      mEntry <- next journal
      replayEntry state mEntry

    replayEntry state Nothing = pure state
    replayEntry state (Just entry)
      | (newState, _) <- react state entry
      = replayLoop newState

    tickFunc = do
      evt <- BChan.readBChan reactorChan
      pure (Just evt)

    config = TrinityConfig
      { tMode = Live
      , tStream = journal
      , tTick = tickFunc
      }

    executor = mkExecutor sock openaiClient reactorChan

-- | Specialized Trinity loop that updates UI — our Com_Frame.
runChatRuntime
  :: TrinityConfig FileJournal ChatEvent
  -> BChan.BChan AppEvent
  -> FileJournal
  -> ChatState
  -> (OutputIntent -> IO ())  -- executor
  -> IO ()
runChatRuntime config uiChan journal startState executor = do
  -- push initial state update immediately
  BChan.writeBChan uiChan (StateUpdate startState)
  comFrame startState

  where
    comFrame state = do
      mEvent <- tTick config
      processEvent state mEvent

    processEvent state Nothing = comFrame state
    processEvent state (Just evt)
      | entry <- Entry 0 0 0 evt
      , (newState, intents) <- react state entry
      = do
        append journal entry
        mapM_ executor intents
        BChan.writeBChan uiChan (StateUpdate newState)
        comFrame newState

mkExecutor
  :: Maybe TcpConnection
  -> Maybe OpenAI.OpenAIClient
  -> BChan.BChan ChatEvent
  -> OutputIntent
  -> IO ()
mkExecutor _ (Just client) reactorChan (QueryLLM prompt) = void $ forkIO $ do
  -- call openrouter api
  let openaiMessages = [OpenAI.ChatMessage "user" (T.pack prompt)]
  result <- OpenAI.complete client openaiMessages
  case result of
    Left err -> BChan.writeBChan reactorChan (AICompletion $ "Error: " <> err)
    Right response -> BChan.writeBChan reactorChan (AICompletion response)

mkExecutor _ Nothing reactorChan (QueryLLM prompt) = void $ forkIO $ do
  -- fallback mock when no api key
  threadDelay 1_500_000
  let response = T.pack $ "[Mock] No OPENAI_API_KEY set. You said: " ++ prompt
  BChan.writeBChan reactorChan (AICompletion response)

mkExecutor Nothing _ _ _ = pure ()  -- offline
mkExecutor (Just conn) _ _ (SendPacket bs) = void $ tcpSend conn bs
mkExecutor _ _ _ (LogMessage msg) = appendFile "chat.log" (msg ++ "\n")
mkExecutor _ _ _ _ = pure ()
