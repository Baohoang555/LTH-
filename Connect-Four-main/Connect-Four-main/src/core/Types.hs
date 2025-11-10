module Core.Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- Người chơi
data Player = Red | Yellow deriving (Eq, Show, Generic)

-- Ô trên bàn cờ
data Cell = Empty | Filled Player deriving (Eq, Show, Generic)

-- Bàn cờ 6 hàng x 7 cột
type Board = [[Cell]]

-- Index cột (0-6)
type ColIndex = Int

-- Trạng thái game
data GameStatus 
  = Playing Player      -- Đang chơi, lượt của ai
  | Won Player          -- Ai thắng
  | Draw                -- Hòa
  deriving (Eq, Show, Generic)

-- Lỗi nước đi
data MoveError
  = NotYourTurn
  | ColumnFull
  | InvalidColumn
  | GameAlreadyOver
  deriving (Eq, Show, Generic)

-- Đổi lượt
nextPlayer :: Player -> Player
nextPlayer Red = Yellow
nextPlayer Yellow = Red

-- Tạo bàn cờ rỗng (6x7)
emptyBoard :: Board
emptyBoard = replicate 6 (replicate 7 Empty)

-- Aeson instances
instance ToJSON Player
instance FromJSON Player
instance ToJSON Cell
instance FromJSON Cell
instance ToJSON GameStatus
instance FromJSON GameStatus
instance ToJSON MoveError
instance FromJSON MoveError