{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Network.Socket as Net
import Network.TLS ()
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Default.Class as Def
import qualified Network.TLS.SessionManager as TLS
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Marshal.Alloc (mallocBytes, free)
import System.IO.Engine.Types
import System.IO.Engine.Posix (makePosixEngine)
import Control.Concurrent.Async (wait)
import System.Posix.Types (Fd(Fd))
import Control.Exception (finally, catch, SomeException, displayException)
import Control.Monad (void)

main :: IO ()
main = do
    putStrLn "Starting debug..."
    engine <- makePosixEngine
    
    let host = "httpbin.org"
        port = "443"
    
    putStrLn $ "Resolving " ++ host ++ ":" ++ port
    addrInfo:_ <- Net.getAddrInfo (Just Net.defaultHints) (Just host) (Just port)
    let addr = Net.addrAddress addrInfo
        family = Net.addrFamily addrInfo
        proto = Net.addrProtocol addrInfo
        sockType = Net.addrSocketType addrInfo

    sock <- Net.socket family sockType proto
    fd <- Fd <$> Net.socketToFd sock
    putStrLn $ "Created socket FD: " ++ show fd
    
    flip finally (Net.close sock) $ do
        putStrLn "Connecting..."
        ticket <- submit engine (Connect fd addr)
        wait ticket
        putStrLn "Connected."
        
        let backend = makeBackend engine fd
        
        params <- do
            sm <- TLS.newSessionManager TLS.defaultConfig
            return $ (TLS.defaultParamsClient host "")
                { TLS.clientShared = Def.def { TLS.sharedSessionManager = sm }
                , TLS.clientSupported = Def.def { TLS.supportedCiphers = TLS.ciphersuite_default }
                , TLS.clientHooks = Def.def { TLS.onServerCertificate = \_ _ _ _ -> return [] } -- Disable cert validation for debug
                , TLS.clientDebug = Def.def { TLS.debugPrintSeed = \_ -> return () } 
                }
        
        ctx <- TLS.contextNew backend params
        
        putStrLn "Starting Handshake..."
        catch (TLS.handshake ctx) $ \(e :: SomeException) -> do
            putStrLn $ "Handshake failed: " ++ displayException e
            -- Re-throw to exit
            error "Handshake failed"

        putStrLn "Handshake success."
        TLS.bye ctx

makeBackend :: Engine -> Fd -> TLS.Backend
makeBackend engine fd = TLS.Backend
    { TLS.backendFlush = return ()
    , TLS.backendClose = do
        putStrLn "backendClose called"
        t <- submit engine (Close fd)
        wait t
    , TLS.backendSend = \bs -> BSU.unsafeUseAsCStringLen bs $ \(ptr, len) -> do
        putStrLn $ "backendSend: " ++ show len ++ " bytes"
        t <- submit engine (Write fd (castPtr ptr) len)
        _ <- wait t
        return ()
    , TLS.backendRecv = \len -> do
        putStrLn $ "backendRecv requested: " ++ show len ++ " bytes"
        ptr <- mallocBytes len
        
        let loop offset remaining = do
                putStrLn $ "  loop: off=" ++ show offset ++ " rem=" ++ show remaining
                t <- submit engine (Read fd (castPtr (ptr `plusPtr` offset)) remaining)
                n <- wait t
                putStrLn $ "  read: " ++ show n
                if n <= 0
                   then return offset
                   else do
                       let newOff = offset + n
                       let newRem = remaining - n
                       if newRem > 0
                          then loop newOff newRem
                          else return len

        total <- loop 0 len
        
        putStrLn $ "backendRecv got total: " ++ show total ++ " bytes"
        if total <= 0
           then do
               free ptr 
               return BSC.empty
           else do
               bs <- BSC.packCStringLen (castPtr ptr, total)
               free ptr
               return bs
    }
