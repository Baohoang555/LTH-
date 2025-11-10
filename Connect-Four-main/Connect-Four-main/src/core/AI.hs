module Core.AI (getBestMove) where

import Core.Types
import Core.Board
import Data.List (maximumBy, transpose)
import Data.Ord (comparing)

-- Tìm nước đi tốt nhất cho AI (depth = 4)
getBestMove :: Player -> Board -> Maybe Int
getBestMove player board =
  case validMoves board of
    [] -> Nothing
    moves -> Just $ fst $ maximumBy (comparing snd) 
                   [(m, minimax 4 False player m board) | m <- moves]

-- Minimax với alpha-beta đơn giản
minimax :: Int -> Bool -> Player -> Int -> Board -> Int
minimax depth isMax player move board =
  case dropPiece move (if isMax then player else nextPlayer player) board of
    Nothing -> -1000
    Just newBoard
      | checkWin player newBoard -> 1000
      | checkWin (nextPlayer player) newBoard -> -1000
      | isFull newBoard -> 0
      | depth == 0 -> evaluate player newBoard
      | otherwise ->
          let moves = validMoves newBoard
              scores = [minimax (depth - 1) (not isMax) player m newBoard | m <- moves]
          in if isMax then maximum scores else minimum scores

-- Hàm đánh giá đơn giản
evaluate :: Player -> Board -> Int
evaluate player board =
  countThrees player board - countThrees (nextPlayer player) board

countThrees :: Player -> Board -> Int
countThrees player board =
  length $ filter (hasThree player) (rows ++ cols ++ diags)
  where
    rows = board
    cols = transpose board
    diags = diagonals board ++ diagonals (map reverse board)

hasThree :: Player -> [Cell] -> Bool
hasThree player cells = 
  any (\w -> count (Filled player) w == 3 && Empty `elem` w) (windows 4 cells)

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

windows :: Int -> [a] -> [[a]]
windows n xs
  | length xs < n = []
  | otherwise = take n xs : windows n (drop 1 xs)

diagonals :: Board -> [[Cell]]
diagonals board = 
  [ [board !! r !! c | (r, c) <- zip [sr..] [sc..], r < 6, c < 7] 
  | sr <- [0..5], sc <- [0..6], sr == 0 || sc == 0 ]