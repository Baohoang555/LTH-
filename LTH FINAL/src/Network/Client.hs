{-# LANGUAGE OverloadedStrings #-}
module Network.Client where

import Control.Concurrent (forkFinally)
import Control.Monad (forever)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Network.WebSockets as WS
import Brick (customMain)
import Brick.BChan (BChan, newBChan, writeBChan)
import qualified Graphics.Vty as V
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception (finally, catch, SomeException)
import Network.Protocol  -- Import Protocol.hs (Message, encodeBinary, decodeBinary, validateMessage, etc.)
import Game.Types (GameState, Player)
import UI.Terminal (connectFourApp, ClientState(..))  -- Giả sử UI.Terminal.hs đã có, với ClientState và connectFourApp

-- | Hàm callback cho WebSocket client: Xử lý kết nối, gửi/nhận messages, và chạy UI Brick.
createClientApp :: Text -> WS.ClientApp (Maybe Text)
createClientApp playerName conn = do
  -- Gửi JoinGame message để join game (sử dụng Binary encoding)
  WS.sendBinaryData conn (encodeBinary $ JoinGame playerName)
  putStrLn $ "Sent JoinGame for: " ++ T.unpack playerName

  -- Nhận message đầu tiên từ server (WaitingForOpponent hoặc GameUpdate)
  res <- WS.receiveBinaryData conn
  case decodeBinary res of
    Nothing -> return $ Just "Invalid binary response from server"  -- Lỗi decode Binary
    Just decoded -> case validateMessage decoded of
      Left err -> return $ Just err  -- Lỗi validation (e.g., invalid playerName)
      Right WaitingForOpponent -> do
        putStrLn "Waiting for opponent..."
        waitForOpponent conn  -- Chờ message tiếp theo
      Right (GameUpdate initialState) -> do
        putStrLn "Game started!"
        startGame conn initialState  -- Bắt đầu game với state ban đầu
      Right (ErrorMsg err) -> return $ Just err  -- Server gửi lỗi
      _ -> return $ Just "Unexpected initial response"  -- Message không hợp lệ

  where
    -- | Hàm chờ opponent: Nhận messages trong lúc chờ (e.g., GameUpdate khi match thành công)
    waitForOpponent :: WS.Connection -> IO (Maybe Text)
    waitForOpponent conn = do
      msg <- WS.receiveBinaryData conn
      case decodeBinary msg of
        Nothing -> return $ Just "Invalid binary message in waiting"
        Just decoded -> case validateMessage decoded of
          Left err -> return $ Just err
          Right (GameUpdate initialState) -> startGame conn initialState  -- Match thành công, bắt đầu game
          Right (ErrorMsg err) -> return $ Just err  -- Lỗi từ server
          _ -> waitForOpponent conn  -- Ignore và chờ tiếp

    -- | Hàm bắt đầu game: Thiết lập channel cho Brick, fork thread lắng nghe server, chạy UI
    startGame :: WS.Connection -> GameState -> IO (Maybe Text)
    startGame conn initialState = do
      chan <- newBChan 10  -- Channel để đẩy events từ server vào Brick UI
      -- Fork thread lắng nghe messages từ server (realtime updates)
      _ <- forkFinally
             (forever $ do
                msg <- WS.receiveBinaryData conn
                case decodeBinary msg of
                  Just decoded -> case validateMessage decoded of
                    Right validMsg -> writeBChan chan validMsg  -- Đẩy message hợp lệ vào channel cho UI xử lý
                    Left err -> putStrLn $ "Validation error: " ++ T.unpack err  -- Log lỗi
                  Nothing -> putStrLn "Invalid binary message from server"  -- Log lỗi decode
             )
             (\_ -> putStrLn "Connection closed or error")  -- Cleanup khi thread end

      -- Xây dựng Vty cho Brick UI
      let buildVty = V.mkVty V.defaultConfig
      initialVty <- buildVty

      -- Chạy Brick app với state ban đầu (Playing với conn để gửi moves từ UI)
      _ <- customMain initialVty buildVty (Just chan) connectFourApp (Playing conn initialState)
      WS.sendClose conn ("Bye!" :: ByteString)  -- Đóng kết nối gracefully
      return Nothing  -- Kết thúc thành công

-- | Hàm chạy client: Kết nối đến server và chạy createClientApp
runClient :: Text -> IO (Maybe Text)
runClient playerName = WS.runClient "localhost" 8080 "/" (createClientApp playerName)
  `catch` (\(e :: SomeException) -> do
    putStrLn $ "Client error: " ++ show e
    return $ Just $ T.pack $ show e
  ) `finally` putStrLn "Client disconnected"