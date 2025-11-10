{-# LANGUAGE OverloadedStrings #-}

module Network.Client (runClient) where

import Core.Types
import UI.Graphics
import Network.Message
import Utils.Parser (parseColumn)
import Network.Socket
import Control.Concurrent (forkIO, MVar, newMVar, readMVar, modifyMVar_)
import Control.Monad (forever)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Exception (bracket, catch, SomeException)
import System.IO

data ClientState = ClientState
  { csMyPlayer :: Maybe Player
  , csCurrentBoard :: Board
  , csGameStatus :: GameStatus
  }

initialClientState :: ClientState
initialClientState = ClientState
  { csMyPlayer = Nothing
  , csCurrentBoard = emptyBoard
  , csGameStatus = Playing Red
  }

runClient :: String -> String -> IO ()
runClient host port = withSocketsDo $ do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin LineBuffering
    
    clearScreen
    putStrLn showWelcomeScreen
    putStrLn $ "Connecting to " ++ host ++ ":" ++ port ++ "..."
    
    let hints = defaultHints { addrSocketType = Stream }
    addrs <- getAddrInfo (Just hints) (Just host) (Just port)
    
    case addrs of
        [] -> putStrLn "Error: Cannot resolve address"
        (addr:_) -> do
            putStrLn "Connected!"
            bracket (connectToServer addr) hClose $ \handle -> do
                stateVar <- newMVar initialClientState
                _ <- forkIO (serverListener handle stateVar)
                clientInput handle stateVar

connectToServer :: AddrInfo -> IO Handle
connectToServer addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr)
    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle LineBuffering
    return handle

serverListener :: Handle -> MVar ClientState -> IO ()
serverListener handle stateVar = forever $ do
    msg <- hGetLine handle `catch` handleError
    case decode (BL.pack msg) :: Maybe ServerMsg of
        Just m -> handleServerMessage stateVar m
        Nothing -> putStrLn $ "Cannot decode: " ++ msg
  where
    handleError :: SomeException -> IO String
    handleError e = do
        putStrLn $ "Connection lost: " ++ show e
        return ""

handleServerMessage :: MVar ClientState -> ServerMsg -> IO ()
handleServerMessage stateVar msg = case msg of
    GameUpdate board status -> do
        modifyMVar_ stateVar $ \s -> return s 
            { csCurrentBoard = board
            , csGameStatus = status
            }
        state <- readMVar stateVar
        displayGame state
        case (status, csMyPlayer state) of
            (Playing turnPlayer, Just myPlayer) 
                | turnPlayer == myPlayer -> do
                    putStr showMovePrompt
                    hFlush stdout
            _ -> return ()
    
    AssignPlayer player -> do
        modifyMVar_ stateVar $ \s -> return s { csMyPlayer = Just player }
        putStrLn $ showPlayerInfo player
        putStrLn "\nWaiting for other player...\n"
    
    NotifyWait str -> putStrLn $ "\n" ++ str ++ "\n"
    
    NotifyTurn player -> do
        state <- readMVar stateVar
        case csMyPlayer state of
            Just myPlayer 
                | player == myPlayer -> do
                    putStrLn "\nYour turn!"
                    putStr showMovePrompt
                    hFlush stdout
                | otherwise -> putStrLn "\nOpponent thinking..."
            Nothing -> return ()
    
    ErrorMove err -> do
        putStrLn $ "\nError: " ++ show err
        putStr showMovePrompt
        hFlush stdout
    
    ErrorInternal err -> putStrLn $ "\nServer error: " ++ err

displayGame :: ClientState -> IO ()
displayGame state = do
    clearScreen
    putStrLn showWelcomeScreen
    case csMyPlayer state of
        Just p -> putStrLn $ "You are: " ++ show p
        Nothing -> return ()
    putStrLn $ showBoardWithColors (csCurrentBoard state)
    putStrLn $ showGameStatus (csGameStatus state)

clientInput :: Handle -> MVar ClientState -> IO ()
clientInput handle stateVar = forever $ do
    input <- getLine
    state <- readMVar stateVar
    case csGameStatus state of
        Won _ -> putStrLn "\nGame over!"
        Draw -> putStrLn "\nGame over!"
        Playing turnPlayer -> do
            case csMyPlayer state of
                Just myPlayer | myPlayer == turnPlayer -> do
                    case parseColumn input of
                        Left err -> do
                            putStrLn $ "\n" ++ err
                            putStr showMovePrompt
                            hFlush stdout
                        Right colIdx -> do
                            sendMessage handle (SendMove colIdx)
                            putStrLn "\nSending move..."
                Just _ -> putStrLn "\nNot your turn!"
                Nothing -> return ()

sendMessage :: Handle -> ClientMsg -> IO ()
sendMessage h msg = do
    hPutStrLn h (BL.unpack $ encode msg) `catch` handleSendError
  where
    handleSendError :: SomeException -> IO ()
    handleSendError e = putStrLn $ "\nCannot send: " ++ show e