{-# LANGUAGE OverloadedStrings #-}

module Network.Client (runClient) where

import Core.Types
import Core.Board (showBoard)  
import Network.Message
import Utils.Parser (parseColumn)
import Network.Socket
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Exception (bracket)
import System.IO (
    Handle, hClose, hSetBuffering, hFlush, stdout,
    IOMode(ReadWriteMode), BufferMode(LineBuffering),
    hGetLine, hPutStrLn
    )

-- Chạy client
runClient :: String -> String -> IO ()
runClient host port = withSocketsDo $ do
    addr <- resolve
    putStrLn $ "Dang ket noi den " ++ host ++ ":" ++ port
    bracket (open addr) hClose $ \handle -> do
        putStrLn "Da ket noi!"
        -- Tạo 2 luồng: 1 để nghe server, 1 để gửi input
        forkIO (serverListener handle) -- Luồng nghe
        clientInput handle              -- Luồng gửi (main thread)
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)

    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock (addrAddress addr)
        handle <- socketToHandle sock ReadWriteMode
        hSetBuffering handle LineBuffering
        return handle

-- Luồng lắng nghe tin nhắn từ server
serverListener :: Handle -> IO ()
serverListener handle = forever $ do
    msg <- hGetLine handle
    case decode (BL.pack msg) :: Maybe ServerMsg of
        Just m -> handleServerMessage m
        Nothing -> putStrLn $ "Khong the giai ma tin nhan: " ++ msg

-- Xử lý tin nhắn nhận được từ server
handleServerMessage :: ServerMsg -> IO ()
handleServerMessage msg = case msg of
    GameUpdate board status -> do
        putStrLn (showBoard board) -- Hiển thị bàn cờ
        printStatus status
    AssignPlayer p ->
        putStrLn $ "Ban la nguoi choi: " ++ show p
    NotifyWait str ->
        putStrLn $ "[THONG BAO] " ++ str
    NotifyTurn p ->
        putStrLn $ ">>> Luot cua: " ++ show p
    ErrorMove err ->
        putStrLn $ "[LOI NUOC DI] " ++ show err
    ErrorInternal err ->
        putStrLn $ "[LOI SERVER] " ++ err

-- In trạng thái game
printStatus :: GameStatus -> IO ()
printStatus (Playing p) = putStrLn $ "Luot cua: " ++ show p
printStatus (Won p) = putStrLn $ "!!! " ++ show p ++ " THANG ROI !!!"
printStatus Draw = putStrLn "!!! HOA !!!"

-- Luồng đọc input từ người dùng
clientInput :: Handle -> IO ()
clientInput handle = forever $ do
    putStr "Nhap cot (0-6) de tha quan: "
    hFlush stdout
    input <- getLine
    
    -- Phân tích input
    case parseColumn input of
        Left err -> putStrLn err
        Right colIdx ->
            -- Gửi nước đi lên server
            sendMessage handle (SendMove colIdx)

-- Hàm gửi tin nhắn (ClientMsg)
sendMessage :: Handle -> ClientMsg -> IO ()
sendMessage h msg = hPutStrLn h (BL.unpack $ encode msg)