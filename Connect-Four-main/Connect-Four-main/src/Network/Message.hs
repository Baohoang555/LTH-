{-# LANGUAGE DeriveGeneric #-}

module Network.Message where

import Core.Types
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- -- TIN NHẮN TỪ CLIENT GỬI LÊN SERVER --

data ClientMsg
    = SendMove ColIndex -- Client gửi một nước đi
    | RequestRestart    -- Client yêu cầu chơi lại (TODO)
    deriving (Show, Eq, Generic)

-- -- TIN NHẮN TỪ SERVER GỬI VỀ CLIENT --

data ServerMsg
    -- Trạng thái
    = GameUpdate Board GameStatus -- Cập nhật bàn cờ và trạng thái
    | AssignPlayer Player         -- Gán người chơi (X hay O) khi mới kết nối

    -- Thông báo
    | NotifyWait String           -- Báo client đợi (ví dụ: "Đợi người chơi khác")
    | NotifyTurn Player           -- Báo lượt của ai

    -- Lỗi
    | ErrorMove MoveError         -- Nước đi không hợp lệ
    | ErrorInternal String        -- Lỗi hệ thống
    deriving (Show, Eq, Generic)


-- -- INSTANCES CHO VIỆC SERIALIZE (JSON) --

instance ToJSON ClientMsg
instance FromJSON ClientMsg

instance ToJSON ServerMsg
instance FromJSON ServerMsg