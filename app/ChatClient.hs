{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Brick
  ( App(App, appDraw, appChooseCursor, appHandleEvent, appStartEvent, appAttrMap)
  , BrickEvent(AppEvent, VtyEvent)
  , EventM
  , Widget
  , customMain
  , showFirstCursor
  )
import Brick.Widgets.Core (vBox, txt, padAll, padLeftRight, strWrap)
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import Lens.Micro ((^.), (.~), (&), (%~))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl (zoom)
import qualified Brick.BChan as BChan
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Control.Monad (void, forever)
import Control.Monad.State (put, get)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Control.Exception (catch, SomeException, displayException, finally, bracket)
import Control.Concurrent.STM (TChan, newTChanIO, readTChan, writeTChan, atomically)

-- Network & Engine Imports
import qualified Network.Socket as Net
import Network.TLS ()
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Default.Class as Def
import qualified Network.TLS.SessionManager as TLS
import System.X509 (getSystemCertificateStore)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Marshal.Alloc (mallocBytes, free)
import System.IO.Engine.Types
  ( Engine(submit)
  , Request(Read, Write, Connect, Close)
  )
import System.IO.Engine.URing (makeURingEngine)
import Control.Concurrent.Async (wait)
import Data.Word ()
import System.Posix.Types (Fd(Fd))

-- Types

data Message = Message
  { _msgRole :: Text
  , _msgContent :: Text
  } deriving (Show, Eq)

data St = St
  { _stMessages :: [Message]
  , _stEditor   :: E.Editor Text ()
  , _stStatus   :: Text
  , _stOutChan  :: TChan Text
  }

makeLenses ''St

data Event = NewMessage Message | ErrorMsg Text

-- TUI

drawUI :: St -> [Widget ()]
drawUI st = [ui]
  where
    ui = C.center $ B.borderWithLabel (txt " Baseten Chat ") $
         vBox [ msgList
              , B.hBorder
              , inputArea
              , B.hBorder
              , statusBar
              ]
    
    msgList = C.centerLayer $ vBox $ map drawMsg (reverse $ st ^. stMessages)
    
    drawMsg (Message role content) = 
      let prefix = if role == "user" then "You: " else "AI: "
      in padLeftRight 1 $ strWrap (prefix ++ Text.unpack content)

    inputArea = padAll 1 $ E.renderEditor (txt . Text.unlines) True (st ^. stEditor)
    
    statusBar = padLeftRight 1 $ txt (st ^. stStatus)

appEvent :: BrickEvent () Event -> EventM () St ()
appEvent (AppEvent (NewMessage msg)) = 
  put . (\st -> st & stMessages %~ (msg :) & stStatus .~ "Ready") =<< get
appEvent (AppEvent (ErrorMsg err)) =
  put . (\st -> st & stStatus .~ ("Error: " <> err)) =<< get
appEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  st <- get
  let content = Text.unlines $ E.getEditContents (st ^. stEditor)
  if Text.null (Text.strip content)
    then return ()
    else do
      -- Send message logic would go here
      let userMsg = Message "user" (Text.strip content)
      put $ st & stMessages %~ (userMsg :) 
               & stEditor .~ E.editor () (Just 1) ""
      -- Send to network
      liftIO $ atomically $ writeTChan (st ^. stOutChan) (Text.strip content)
appEvent (VtyEvent e) = do
  zoom stEditor $ E.handleEditorEvent (VtyEvent e)
appEvent _ = return ()

initialState :: TChan Text -> St
initialState ch = St
  { _stMessages = []
  , _stEditor = E.editor () (Just 1) ""
  , _stStatus = "Ready"
  , _stOutChan = ch
  }

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr []

app :: App St Event ()
app = App
  { appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = appEvent
  , appStartEvent = return ()
  , appAttrMap = const theMap
  }

-- Main

main :: IO ()
main = do
  chan <- BChan.newBChan 10
  outChan <- newTChanIO
  
  -- Start the Engine-powered Network Loop
  void $ forkIO $ networkLoop chan outChan

  vty <- VCP.mkVty V.defaultConfig
  void $ customMain vty (VCP.mkVty V.defaultConfig) (Just chan) app (initialState outChan)

-- Network Logic using System.IO.Engine
networkLoop :: BChan.BChan Event -> TChan Text -> IO ()
networkLoop chan outChan = handleErrors $ do
    -- 1. Initialize the Engine (Posix for now, swap to URing later)
    engine <- makeURingEngine
    
    -- 2. Resolve Host (Baseten / Httpbin)
    -- Using httpbin for echo test, port 443 for TLS
    let host = "httpbin.org"
        port = "443"
    
    addrInfo:_ <- Net.getAddrInfo (Just Net.defaultHints) (Just host) (Just port)
    let addr = Net.addrAddress addrInfo
        family = Net.addrFamily addrInfo
        proto = Net.addrProtocol addrInfo
        sockType = Net.addrSocketType addrInfo

    -- 3. Create Socket via Engine (Using bare socket for now to get Fd)
    -- Note: Our Engine abstraction has Connect but not Socket creation yet in the Request type
    -- (The Request type has Accept/Connect taking Fd). 
    -- So we create socket via GHC/Network, then handover.
    sock <- Net.socket family sockType proto
    fd <- Fd <$> Net.socketToFd sock
    
    flip finally (Net.close sock) $ do
        -- 4. Connect via Engine
        ticket <- submit engine (Connect fd addr)
        wait ticket
        
        let backend = makeBackend engine fd
        
        params <- do
            sm <- TLS.newSessionManager TLS.defaultConfig
            certStore <- getSystemCertificateStore
            return $ (TLS.defaultParamsClient host "")
                { TLS.clientShared = Def.def 
                    { TLS.sharedSessionManager = sm 
                    , TLS.sharedCAStore = certStore
                    }
                , TLS.clientSupported = Def.def { TLS.supportedCiphers = TLS.ciphersuite_default }
                }
        
        ctx <- TLS.contextNew backend params
        TLS.handshake ctx
        
        -- 6. Send Initial Request (HTTP POST)
        let req = "POST /post HTTP/1.1\r\n" <>
                  "Host: " <> BSC.pack host <> "\r\n" <>
                  "Content-Type: text/plain\r\n" <>
                  "Content-Length: 12\r\n" <>
                  "Connection: keep-alive\r\n\r\n" <>
                  "Hello Engine"
        
        TLS.sendData ctx (LBS.fromStrict $ BSC.concat [req])
        
        -- 7. Network Loop: Read from Socket -> UI, Read from UI -> Socket
        -- We spawn a reader thread for the socket
        void $ forkIO $ forever $ do
            -- Block reading from TLS
            msg <- TLS.recvData ctx
            if BSC.null msg 
               then return () 
               else BChan.writeBChan chan (NewMessage $ Message "ai" (Text.decodeUtf8 msg))
        
        -- Write loop
        forever $ do
            msg <- atomically $ readTChan outChan
            let body = BSC.unpack $ Text.encodeUtf8 msg
                httpReq = "POST /post HTTP/1.1\r\n" <>
                          "Host: " <> host <> "\r\n" <>
                          "Content-Length: " <> show (length body) <> "\r\n\r\n" <>
                          body
            TLS.sendData ctx (LBS.fromStrict $ BSC.pack httpReq)

    where
      handleErrors :: IO () -> IO ()
      handleErrors action = catch action $ \(e :: SomeException) -> do
          let err = Text.pack $ displayException e
          BChan.writeBChan chan (ErrorMsg err)
          -- Also print to stderr just in case
          liftIO $ putStrLn $ "Network Error: " ++ displayException e

-- | Bridge between TLS and our IO Engine
makeBackend :: Engine -> Fd -> TLS.Backend
makeBackend engine fd = TLS.Backend
    { TLS.backendFlush = return ()
    , TLS.backendClose = do
        t <- submit engine (Close fd)
        wait t
    , TLS.backendSend = \bs -> BSU.unsafeUseAsCStringLen bs $ \(ptr, len) -> do
        t <- submit engine (Write fd (castPtr ptr) len)
        _ <- wait t
        return ()
    , TLS.backendRecv = \len -> do
        bracket (mallocBytes len) free $ \ptr -> do
            let loop offset remaining = do
                    t <- submit engine (Read fd (castPtr (ptr `plusPtr` offset)) remaining)
                    n <- wait t
                    if n <= 0
                       then return offset
                       else do
                           let newOff = offset + n
                           let newRem = remaining - n
                           if newRem > 0
                              then loop newOff newRem
                              else return len
            
            total <- loop 0 len
            if total <= 0
               then return BSC.empty
               else BSC.packCStringLen (castPtr ptr, total)
    }
