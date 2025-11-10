{-# LANGUAGE OverloadedStrings #-}
module Network.Server (runServer) where

import Core.Types
import Core.Board
import Network.Message
import Network.Socket
import System.IO
import Control.Concurrent (forkIO, MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad (forever)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Exception (finally, bracket, catch, SomeException)

-- Trạng thái server đơn giản
data ServerState = ServerState
  { ssBoard :: Board
  , ssTurn :: Player
  , ssPlayerX :: Maybe Handle
  , ssPlayerO :: Maybe Handle
  }

runServer :: String -> IO ()
runServer port = withSocketsDo $ do
    addr <- resolve port
    putStrLn $ "Server dang nghe o cong " ++ port
    bracket (open addr) close loop
  where
    resolve p = do
        let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) Nothing (Just p)
    
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        listen sock 2
        return sock
    
    loop sock = do
        let initState = ServerState emptyBoard Red Nothing Nothing
        stateVar <- newMVar initState
        
        (h1, _) <- accept sock
        handle1 <- socketToHandle h1 ReadWriteMode
        hSetBuffering handle1 LineBuffering
        putStrLn "Player 1 ket noi"
        
        modifyMVar_ stateVar $ \s -> return s { ssPlayerX = Just handle1 }
        hPutStrLn handle1 (BL.unpack $ encode $ AssignPlayer Red)
        
        (h2, _) <- accept sock
        handle2 <- socketToHandle h2 ReadWriteMode
        hSetBuffering handle2 LineBuffering
        putStrLn "Player 2 ket noi"
        
        modifyMVar_ stateVar $ \s -> return s { ssPlayerO = Just handle2 }
        hPutStrLn handle2 (BL.unpack $ encode $ AssignPlayer Yellow)
        
        putStrLn "Game bat dau!"
        broadcast stateVar
        
        forkIO $ handleClient stateVar handle1 Red
        handleClient stateVar handle2 Yellow

handleClient :: MVar ServerState -> Handle -> Player -> IO ()
handleClient stateVar h player = forever $ do
    line <- hGetLine h
    case decode (BL.pack line) of
      Just (SendMove col) -> processMove stateVar player col
      _ -> return ()

processMove :: MVar ServerState -> Player -> Int -> IO ()
processMove stateVar player col = do
    modifyMVar_ stateVar $ \s -> do
        if ssTurn s /= player then return s
        else case dropPiece col player (ssBoard s) of
          Nothing -> return s
          Just newBoard ->
            let newTurn = if player == Red then Yellow else Red
            in return s { ssBoard = newBoard, ssTurn = newTurn }
    broadcast stateVar

broadcast :: MVar ServerState -> IO ()
broadcast stateVar = do
    s <- readMVar stateVar
    let status = if checkWin Red (ssBoard s) then Won Red
                 else if checkWin Yellow (ssBoard s) then Won Yellow
                 else if isFull (ssBoard s) then Draw
                 else Playing (ssTurn s)
    let msg = encode $ GameUpdate (ssBoard s) status
    case ssPlayerX s of Just h -> hPutStrLn h (BL.unpack msg); _ -> return ()
    case ssPlayerO s of Just h -> hPutStrLn h (BL.unpack msg); _ -> return ()