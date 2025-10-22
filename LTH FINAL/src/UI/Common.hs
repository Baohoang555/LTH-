-- Common.hs (common widgets: draw board, finished, etc.)
{-# LANGUAGE TemplateHaskell #-}

module UI.Common where

import Brick
  ( Widget, hBox, vBox, txt, withAttr, str, (<+>), (<=>), emptyWidget, ResourceName
  )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Control.Lens ((^.))
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Game.Types (Board, GameState(..), Player(..), gameStatus, board, currentPlayer, moveCount, Cell(..), getCell)  -- Import from your Types.hs
import Game.Rules (findWinningLine, WinInfo(..))  -- For win highlight

-- Helper to get positions for a row (fixed 7 columns)
rowPositions :: Int -> [(Int, Int)]
rowPositions row = [(col, row) | col <- [0..6]]

titleWidget :: Widget n
titleWidget = txt "Four in a Row"  -- Adapt if need logo

drawFinished :: GameState -> Text -> Widget ResourceName
drawFinished gs help = case gs^.gameStatus of
  InProgress -> emptyWidget
  _ -> C.centerLayer $ B.borderWithLabel (txt "Game Over") stats <=> instructions
  where
    stats = txt ("Winner: " ++ show (gs^.gameStatus)) <=> txt ("Moves: " ++ show (gs^.moveCount))
    instructions = C.hCenter (txt help)

drawBoard :: GameState -> Widget ResourceName
drawBoard gs = B.border $ vBox [ hBox [ cell pos | pos <- rowPositions row ] | row <- [5,4..0] ]  -- Bottom to top
  where
    cell pos@(col, row) = let c = getCell (gs^.board) pos  -- Use your getCell
                              attr = if isWinningPos pos then winAttr else getCellAttr c
                          in withAttr attr (txt (showCellEmoji c) <+> txt " ")
    getCellAttr Empty = primaryAttr
    getCellAttr (Occupied Red) = redAttr
    getCellAttr (Occupied Black) = blackAttr
    isWinningPos pos = case findWinningLine (gs^.board) of
                         Just wi -> pos `elem` winningPositions wi
                         Nothing -> False
    showCellEmoji Empty = "○"
    showCellEmoji (Occupied Red) = "●"
    showCellEmoji (Occupied Black) = "●"

drawTurnIndicator :: Player -> Widget ResourceName
drawTurnIndicator p = txt ("Current turn: " ++ show p)

addBorder :: Text -> Widget ResourceName -> Widget ResourceName
addBorder t = B.borderWithLabel (txt t)

listDrawElement :: Bool -> Text -> Widget n
listDrawElement sel t = C.hCenter $ txt (if sel then "* " else "  ") <+> txt t