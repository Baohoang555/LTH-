module UI.Graphics (showBoard) where

import Core.Types
import Data.List (transpose, intersperse)

-- Hiển thị bàn cờ (Board) thành một Chuỗi (String)
showBoard :: Board -> String
showBoard board =
    let
        -- Chuyển đổi Cell thành ký tự
        showCell :: Cell -> Char
        showCell Empty = '.'
        showCell (Occupied PlayerX) = 'X'
        showCell (Occupied PlayerO) = 'O'
        
        -- Chuyển đổi bàn cờ (danh sách các cột)
        -- thành danh sách các hàng để in
        rows = transpose board
        
        -- Chuyển các hàng Cell thành hàng String
        stringRows = map (intersperse ' ' . map showCell) (reverse rows) -- In từ dưới lên
        
        -- Thêm chỉ số cột ở dưới
        header = "0 1 2 3 4 5 6"
    in
    unlines (stringRows ++ [header])