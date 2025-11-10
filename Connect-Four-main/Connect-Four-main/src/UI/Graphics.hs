module UI.Graphics 
  ( showBoard
  , showBoardWithColors
  , showGameStatus
  , showWelcomeScreen
  , showPlayerInfo
  , clearScreen
  , showMovePrompt
  ) where

import Core.Types
import Data.List (transpose, intersperse, intercalate)

-- =====================================
-- CONSTANTS FOR DISPLAY
-- =====================================

emptyChar :: String
emptyChar = "."

redChar :: String
redChar = "R"

yellowChar :: String
yellowChar = "Y"

topBorder :: String
topBorder = "+---+---+---+---+---+---+---+"

midBorder :: String
midBorder = "+---+---+---+---+---+---+---+"

bottomBorder :: String
bottomBorder = "+---+---+---+---+---+---+---+"

columnNumbers :: String
columnNumbers = "  0   1   2   3   4   5   6  "

-- =====================================
-- MAIN DISPLAY FUNCTIONS
-- =====================================

showBoard :: Board -> String
showBoard board =
    unlines $ 
        [ topBorder ] ++
        intersperse midBorder (map showRow rows) ++
        [ bottomBorder, columnNumbers ]
  where
    rows = reverse (transpose board)
    
    showRow :: [Cell] -> String
    showRow cells = "| " ++ intercalate " | " (map showCell cells) ++ " |"
    
    showCell :: Cell -> String
    showCell Empty = " "
    showCell (Filled Red) = "R"
    showCell (Filled Yellow) = "Y"

showBoardWithColors :: Board -> String
showBoardWithColors board =
    unlines $ 
        [ "", "    " ++ topBorder ] ++
        map ("    " ++) (intersperse midBorder (map showRowColor rows)) ++
        [ "    " ++ bottomBorder, "    " ++ columnNumbers, "" ]
  where
    rows = reverse (transpose board)
    
    showRowColor :: [Cell] -> String
    showRowColor cells = "| " ++ intercalate " | " (map showCellColor cells) ++ " |"
    
    showCellColor :: Cell -> String
    showCellColor Empty = emptyChar
    showCellColor (Filled Red) = redChar
    showCellColor (Filled Yellow) = yellowChar

-- =====================================
-- GAME STATUS DISPLAY
-- =====================================

showGameStatus :: GameStatus -> String
showGameStatus status = case status of
    Playing player -> 
        "\n" ++ boxMessage ("Turn: " ++ playerName player) ++ "\n"
    Won player -> 
        "\n" ++ boxMessage (">>> " ++ playerName player ++ " WINS! <<<") ++ "\n"
    Draw -> 
        "\n" ++ boxMessage ">>> DRAW <<<" ++ "\n"
  where
    playerName Red = "Red (R)"
    playerName Yellow = "Yellow (Y)"

boxMessage :: String -> String
boxMessage msg = 
    let len = length msg + 4
        border = replicate len '='
    in unlines 
        [ border
        , "| " ++ msg ++ " |"
        , border
        ]

showPlayerInfo :: Player -> String
showPlayerInfo player = 
    boxMessage $ "You are: " ++ playerSymbol player ++ " (" ++ playerName player ++ ")"
  where
    playerSymbol Red = "R"
    playerSymbol Yellow = "Y"
    playerName Red = "RED"
    playerName Yellow = "YELLOW"

showWelcomeScreen :: String
showWelcomeScreen = unlines
    [ ""
    , "========================================"
    , "|                                      |"
    , "|        CONNECT FOUR GAME            |"
    , "|                                      |"
    , "|          4 in a row                 |"
    , "|                                      |"
    , "========================================"
    , ""
    ]

showMovePrompt :: String
showMovePrompt = "\n> Enter column (0-6): "

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"