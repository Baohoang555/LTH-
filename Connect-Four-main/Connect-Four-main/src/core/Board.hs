module Core.Board 
  ( dropPiece
  , checkWin
  , isFull
  , validMoves
  , showBoard
  ) where

import Core.Types
import Data.List (transpose, intercalate)

-- Thả quân vào cột (trả về Nothing nếu cột đầy)
dropPiece :: Int -> Player -> Board -> Maybe Board
dropPiece col player board
  | col < 0 || col >= 7 = Nothing
  | otherwise = 
      let column = transpose board !! col
          emptyRow = length (takeWhile (== Empty) (reverse column))
      in if emptyRow == 0 
         then Nothing
         else Just $ updateBoard (6 - emptyRow) col player board

updateBoard :: Int -> Int -> Player -> Board -> Board
updateBoard row col player board =
  let (before, current:after) = splitAt row board
      newRow = take col current ++ [Filled player] ++ drop (col + 1) current
  in before ++ [newRow] ++ after

-- Kiểm tra thắng
checkWin :: Player -> Board -> Bool
checkWin player board =
  any (hasFour player) (rows ++ cols ++ diags)
  where
    rows = board
    cols = transpose board
    diags = diagonals board ++ diagonals (map reverse board)

hasFour :: Player -> [Cell] -> Bool
hasFour player cells = any (all (== Filled player)) (windows 4 cells)

windows :: Int -> [a] -> [[a]]
windows n xs
  | length xs < n = []
  | otherwise = take n xs : windows n (drop 1 xs)

diagonals :: Board -> [[Cell]]
diagonals board = 
  [ [board !! r !! c | (r, c) <- zip [sr..] [sc..], r < 6, c < 7] 
  | sr <- [0..5], sc <- [0..6], sr == 0 || sc == 0 ]

-- Bàn cờ đầy
isFull :: Board -> Bool
isFull board = null (validMoves board)

-- Các nước đi hợp lệ
validMoves :: Board -> [Int]
validMoves board = [c | c <- [0..6], Empty `elem` (transpose board !! c)]

-- Hiển thị bàn cờ
showBoard :: Board -> String
showBoard board = 
  unlines (map showRow board) ++ " 0 1 2 3 4 5 6"
  where
    showRow = intercalate "" . map showCell
    showCell Empty = "."
    showCell (Filled Red) = "R"
    showCell (Filled Yellow) = "Y"