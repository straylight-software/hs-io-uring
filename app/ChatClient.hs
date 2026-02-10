{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Brick
  ( App(App, appDraw, appChooseCursor, appHandleEvent, appStartEvent, appAttrMap)
  , BrickEvent(AppEvent, VtyEvent)
  , EventM
  , Widget
  , customMain
  , showFirstCursor
  , get
  , halt
  )
import Brick.Widgets.Core (vBox, txt, padAll, padLeftRight, strWrap)
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import qualified Brick.BChan as BChan
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Monad (void, forever)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl (zoom, (.=))
import Network.Simple.TCP (connect, recv, send, Socket)
import Control.Exception (try, SomeException)
-- import qualified Data.ByteString as BS

-- Reactor Imports
import System.IO.EventStream 
  ( StreamMode(Live)
  , Entry(Entry)
  , EventStream(append, next)
  )
import System.IO.EventStream.Journal (openJournal, FileJournal)
import System.IO.Reactor (Reactor(react, initialState), OutputIntent(SendPacket, LogMessage))
import System.IO.Runtime (RuntimeConfig(RuntimeConfig, mode, stream, tick))
import System.IO (IOMode(ReadWriteMode))
import Chat.Logic 
  ( ChatState(messages, status)
  , ChatEvent(UserInput, NetReceived, NetError)
  , Message(Message)
  )

-- | UI State (Ephemeral, Visual only)
-- The "Real" state is in the Reactor, but we need a view model for Brick
data UIState = UIState
  { _uiMessages :: [Message]
  , _uiEditor   :: E.Editor Text ()
  , _uiStatus   :: Text
  }

makeLenses ''UIState

-- | Brick Event Wrapper
data AppEvent 
  = StateUpdate ChatState -- ^ Reactor pushed a new state
  -- | ReactEvent ChatEvent  -- ^ UI wants to push an event to Reactor (Removed, now direct BChan write)

-- | The Brick App Definition
drawUI :: UIState -> [Widget ()]
drawUI st = [ui]
  where
    ui = C.center $ B.borderWithLabel (txt " Aleph Chat (Replay Architecture) ") $
         vBox [ msgList
              , B.hBorder
              , inputArea
              , B.hBorder
              , statusBar
              ]
    
    msgList = C.centerLayer $ vBox $ map drawMsg (reverse $ st ^. uiMessages)
    
    drawMsg (Message role content) = 
      let prefix = if role == "user" then "You: " else "AI: "
      in padLeftRight 1 $ strWrap (prefix ++ T.unpack content)

    inputArea = padAll 1 $ E.renderEditor (txt . T.unlines) True (st ^. uiEditor)
    
    statusBar = padLeftRight 1 $ txt (st ^. uiStatus)

appEvent :: BChan.BChan ChatEvent -> BChan.BChan AppEvent -> BrickEvent () AppEvent -> EventM () UIState ()
appEvent _ _ (AppEvent (StateUpdate newState)) = do
    uiMessages .= messages newState
    uiStatus .= status newState

appEvent reactorChan _ (VtyEvent (V.EvKey V.KEnter [])) = do
  st <- get
  let content = T.unlines $ E.getEditContents (st ^. uiEditor)
  if T.null (T.strip content)
    then return ()
    else do
      -- Push event to Reactor Channel
      liftIO $ BChan.writeBChan reactorChan (UserInput (T.strip content))
      uiEditor .= E.editor () (Just 1) ""

appEvent _ _ (VtyEvent (V.EvKey V.KEsc [])) = do
    halt

appEvent _ _ (VtyEvent e) = do
  zoom uiEditor $ E.handleEditorEvent (VtyEvent e)

appEvent _ _ _ = return ()

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
  , appStartEvent = return ()
  , appAttrMap = const theMap
  }

main :: IO ()
main = do
  -- 1. Setup Channels
  -- uiChan: For Reactor -> UI updates (AppEvent)
  uiChan <- BChan.newBChan 10
  -- reactorChan: For UI -> Reactor inputs (ChatEvent)
  reactorChan <- BChan.newBChan 10 

  -- 2. Open Journal
  journal <- openJournal "chat.journal" ReadWriteMode

  -- 3. Network Setup (Try to connect)
  appendFile "debug.log" "Main: Attempting connection...\n"
  connResult <- try $ connect "127.0.0.1" "8080" $ \(sock, remoteAddr) -> do
      appendFile "debug.log" $ "Main: Connected to " ++ show remoteAddr ++ "\n"
      
      -- 3.1 Spawn Input Pump
      inputPumpId <- forkIO $ forever $ do
          msg <- recv sock 4096
          case msg of
              Nothing -> do
                  appendFile "debug.log" "Main: Socket EOF\n"
                  BChan.writeBChan reactorChan (NetError "Connection Closed")
                  threadDelay 10000000 
                  error "Socket EOF" 
              Just bs -> do
                  appendFile "debug.log" $ "Main: Recv bytes: " ++ show bs ++ "\n"
                  BChan.writeBChan reactorChan (NetReceived (T.decodeUtf8 bs)) 
      
      runAppWithNetwork (Just sock) uiChan reactorChan journal
      
      -- Cleanup
      killThread inputPumpId

  case connResult of
      Left (e :: SomeException) -> do
          appendFile "debug.log" $ "Main: Connection failed: " ++ show e ++ "\n"
          runAppWithNetwork Nothing uiChan reactorChan journal
      Right _ -> return ()

runAppWithNetwork :: Maybe Socket -> BChan.BChan AppEvent -> BChan.BChan ChatEvent -> FileJournal -> IO ()
runAppWithNetwork sock uiChan reactorChan journal = do
  -- 2.5 Replay History
  putStrLn "Replaying journal..."
  let replayLoop state = do
        mEntry <- next journal
        case mEntry of
          Nothing -> return state
          Just entry -> do
            let (newState, _) = react state entry 
            replayLoop newState
  
  initialStateReplayed <- replayLoop (initialState :: ChatState)
  putStrLn $ "Replay complete. Messages: " ++ show (length (messages initialStateReplayed))

  -- 3. Configure Runtime
  -- The 'tick' function pulls from the reactorChan (UI inputs)
  let tickFunc = do
        evt <- BChan.readBChan reactorChan
        return (Just evt)

  let config = RuntimeConfig
        { mode = Live
        , stream = journal
        , tick = tickFunc
        }

  -- 4. Fork the Runtime Loop
  -- Pass the executor (Network Sender)
  let executor = mkExecutor sock
  void $ forkIO $ runChatRuntime config uiChan journal initialStateReplayed executor

  -- 5. Start Brick
  let initialUIReplayed = initialUI { _uiMessages = messages initialStateReplayed }
  vty <- VCP.mkVty V.defaultConfig
  void $ customMain vty (VCP.mkVty V.defaultConfig) (Just uiChan) (app reactorChan uiChan) initialUIReplayed


-- | Specialized Runtime Loop that updates UI
runChatRuntime :: RuntimeConfig FileJournal ChatEvent 
               -> BChan.BChan AppEvent 
               -> FileJournal 
               -> ChatState 
               -> (OutputIntent -> IO ()) -- ^ The Executor
               -> IO ()
runChatRuntime config uiChan journal startState executor = do
    
    -- PUSH INITIAL STATE UPDATE IMMEDIATELY
    BChan.writeBChan uiChan (StateUpdate startState)
    appendFile "debug.log" "Runtime: Started loop\n"

    let loop state = do
          -- Poll Input
          appendFile "debug.log" "Runtime: Waiting for tick...\n"
          mEvent <- tick config
          case mEvent of
             Nothing -> do
                 appendFile "debug.log" "Runtime: Tick returned Nothing\n"
                 loop state
             Just evt -> do
                 appendFile "debug.log" $ "Runtime: Got event: " ++ show evt ++ "\n"
                 
                 -- Persist
                 let entry = Entry 0 0 0 evt
                 append journal entry
                 appendFile "debug.log" "Runtime: Persisted to journal\n"
                 
                 -- React
                 let (newState, intents) = react state entry
                 appendFile "debug.log" $ "Runtime: New State msg count: " ++ show (length (messages newState)) ++ "\n"
                 
                 -- Execute Intents (Real Network!)
                 mapM_ executor intents
                 
                 -- Update UI
                 BChan.writeBChan uiChan (StateUpdate newState)
                 appendFile "debug.log" "Runtime: Pushed UI update\n"
                 
                 loop newState
    
    loop startState

mkExecutor :: Maybe Socket -> OutputIntent -> IO ()
mkExecutor Nothing _ = return () -- Offline
mkExecutor (Just sock) (SendPacket bs) = send sock bs
mkExecutor _ (LogMessage msg) = appendFile "chat.log" (msg ++ "\n")
mkExecutor _ _ = return ()

