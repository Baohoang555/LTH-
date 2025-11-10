module UI.Utils
  ( clearScreen
  , setCursor
  , hideCursor
  , showCursor
  , delay
  , printCenter
  , printBorder
  , printSeparator
  , colorText
  , Color(..)
  ) where

import Control.Concurrent (threadDelay)
import System.Console.ANSI
    ( clearScreen
    , setCursorPosition
    , hideCursor
    , showCursor
    , setSGR
    , SGR(..)
    , ConsoleLayer(..)
    , ColorIntensity(..)
    , Color(..)
    )

-- =====================================
-- ANSI COLOR SUPPORT
-- =====================================

data TextColor
    = ColorRed
    | ColorYellow
    | ColorGreen
    | ColorBlue
    | ColorCyan
    | ColorMagenta
    | ColorWhite
    | ColorDefault
    deriving (Eq, Show)

-- Apply color to text
colorText :: TextColor -> String -> String
colorText color text = colorCode color ++ text ++ resetCode
  where
    resetCode = "\ESC[0m"
    colorCode c = case c of
        ColorRed     -> "\ESC[31m"
        ColorYellow  -> "\ESC[33m"
        ColorGreen   -> "\ESC[32m"
        ColorBlue    -> "\ESC[34m"
        ColorCyan    -> "\ESC[36m"
        ColorMagenta -> "\ESC[35m"
        ColorWhite   -> "\ESC[37m"
        ColorDefault -> "\ESC[39m"

-- Bold text
boldText :: String -> String
boldText text = "\ESC[1m" ++ text ++ "\ESC[0m"

-- =====================================
-- CURSOR CONTROL
-- =====================================

-- Set cursor to specific position (row, col)
setCursor :: Int -> Int -> IO ()
setCursor row col = setCursorPosition row col

-- Move cursor up
cursorUp :: Int -> IO ()
cursorUp n = putStr $ "\ESC[" ++ show n ++ "A"

-- Move cursor down
cursorDown :: Int -> IO ()
cursorDown n = putStr $ "\ESC[" ++ show n ++ "B"

-- =====================================
-- TIMING UTILITIES
-- =====================================

-- Delay in milliseconds
delay :: Int -> IO ()
delay ms = threadDelay (ms * 1000)

-- =====================================
-- DRAWING UTILITIES
-- =====================================

-- Print text centered in terminal (assuming 80 width)
printCenter :: String -> IO ()
printCenter text = do
    let width = 80
        padding = (width - length text) `div` 2
    putStrLn $ replicate padding ' ' ++ text

-- Print a horizontal border
printBorder :: Char -> Int -> IO ()
printBorder char width = putStrLn $ replicate width char

-- Print separator line
printSeparator :: IO ()
printSeparator = printBorder '─' 60

-- Print double separator
printDoubleSeparator :: IO ()
printDoubleSeparator = printBorder '═' 60

-- =====================================
-- BOX DRAWING
-- =====================================

-- Draw a box around text
drawBox :: [String] -> String
drawBox lines =
    let maxLen = maximum (map length lines)
        topBorder = "╔" ++ replicate (maxLen + 2) '═' ++ "╗"
        bottomBorder = "╚" ++ replicate (maxLen + 2) '═' ++ "╝"
        padLine line = "║ " ++ line ++ replicate (maxLen - length line) ' ' ++ " ║"
    in unlines $ [topBorder] ++ map padLine lines ++ [bottomBorder]

-- Simple box message
simpleBox :: String -> String
simpleBox msg = drawBox [msg]

-- Multi-line box
multiLineBox :: [String] -> String
multiLineBox = drawBox

-- =====================================
-- PROGRESS INDICATORS
-- =====================================

-- Show loading spinner
showSpinner :: String -> IO ()
showSpinner msg = do
    let spinChars = ['|', '/', '-', '\\']
    mapM_ (\c -> do
        putStr $ "\r" ++ msg ++ " " ++ [c]
        delay 100) spinChars

-- Progress bar
progressBar :: Int -> Int -> String
progressBar current total =
    let percentage = (current * 100) `div` total
        barLength = 20
        filled = (percentage * barLength) `div` 100
        bar = replicate filled '█' ++ replicate (barLength - filled) '░'
    in "[" ++ bar ++ "] " ++ show percentage ++ "%"

-- =====================================
-- MENU HELPERS
-- =====================================

-- Display menu and get selection
displayMenu :: String -> [String] -> IO Int
displayMenu title options = do
    clearScreen
    printCenter (boldText title)
    printSeparator
    putStrLn ""
    mapM_ (\(i, opt) -> putStrLn $ "  " ++ show i ++ ". " ++ opt)
          (zip [1..] options)
    putStrLn ""
    printSeparator
    putStr $ "\n➤ Chọn (1-" ++ show (length options) ++ "): "
    input <- getLine
    case reads input :: [(Int, String)] of
        [(n, "")] | n >= 1 && n <= length options -> return n
        _ -> do
            putStrLn "\n❌ Lựa chọn không hợp lệ!"
            delay 1000
            displayMenu title options


-- =====================================
-- CONFIRMATION DIALOGS
-- =====================================

-- Yes/No confirmation
confirm :: String -> IO Bool
confirm question = do
    putStr $ question ++ " (y/n): "
    answer <- getLine
    return $ answer `elem` ["y", "Y", "yes", "Yes", "YES"]

-- =====================================
-- TABLE DRAWING
-- =====================================

-- Draw a simple table
drawTable :: [String] -> [[String]] -> String
drawTable headers rows =
    let colWidths = map maximum $ transpose $
            map (map length) (headers : rows)

        makeSeparator = "+" ++ concatMap (\w -> replicate (w + 2) '-' ++ "+") colWidths

        makeRow cells = "|" ++ concat
            (zipWith (\cell width ->
                " " ++ cell ++ replicate (width - length cell) ' ' ++ " |")
             cells colWidths)

        headerRow = makeRow headers
        dataRows = map makeRow rows

    in unlines $
       [makeSeparator, headerRow, makeSeparator] ++
       dataRows ++
       [makeSeparator]
  where
    transpose [] = []
    transpose ([]:_) = []
    transpose xs = map head xs : transpose (map tail xs)

-- =====================================
-- ANIMATION HELPERS
-- =====================================

-- Type writer effect
typeWriter :: String -> Int -> IO ()
typeWriter [] _ = putStrLn ""
typeWriter (c:cs) delayMs = do
    putChar c
    delay delayMs
    typeWriter cs delayMs

-- Fade in effect (simulated)
fadeIn :: String -> IO ()
fadeIn text = do
    typeWriter text 30

-- =====================================
-- ERROR AND SUCCESS MESSAGES
-- =====================================

-- Show error message
showError :: String -> IO ()
showError msg = do
    putStrLn $ colorText ColorRed $ "❌ LỖI: " ++ msg

-- Show success message
showSuccess :: String -> IO ()
showSuccess msg = do
    putStrLn $ colorText ColorGreen $ "✅ " ++ msg

-- Show warning message
showWarning :: String -> IO ()
showWarning msg = do
    putStrLn $ colorText ColorYellow $ "⚠️  CẢNH BÁO: " ++ msg

-- Show info message
showInfo :: String -> IO ()
showInfo msg = do
    putStrLn $ colorText ColorCyan $ "ℹ️  " ++ msg

-- =====================================
-- ASCII ART HELPERS
-- =====================================

-- Connect Four logo
asciiLogo :: String
asciiLogo = unlines
    [ "  ██████╗ ██████╗ ███╗   ██╗███╗   ██╗███████╗ ██████╗████████╗    ██╗  ██╗"
    , " ██╔════╝██╔═══██╗████╗  ██║████╗  ██║██╔════╝██╔════╝╚══██╔══╝    ██║  ██║"
    , " ██║     ██║   ██║██╔██╗ ██║██╔██╗ ██║█████╗  ██║        ██║       ███████║"
    , " ██║     ██║   ██║██║╚██╗██║██║╚██╗██║██╔══╝  ██║        ██║       ╚════██║"
    , " ╚██████╗╚██████╔╝██║ ╚████║██║ ╚████║███████╗╚██████╗   ██║            ██║"
    , "  ╚═════╝ ╚═════╝ ╚═╝  ╚═══╝╚═╝  ╚═══╝╚══════╝ ╚═════╝   ╚═╝            ╚═╝"
    ]

-- Victory banner
victoryBanner :: String
victoryBanner = unlines
    [ "  ██╗   ██╗██╗ ██████╗████████╗ ██████╗ ██████╗ ██╗  ██╗"
    , "  ██║   ██║██║██╔════╝╚══██╔══╝██╔═══██╗██╔══██╗╚██╗ ██╔╝"
    , "  ██║   ██║██║██║        ██║   ██║   ██║██████╔╝ ╚████╔╝ "
    , "  ╚██╗ ██╔╝██║██║        ██║   ██║   ██║██╔══██╗  ╚██╔╝  "
    , "   ╚████╔╝ ██║╚██████╗   ██║   ╚██████╔╝██║  ██║   ██║   "
    , "    ╚═══╝  ╚═╝ ╚═════╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝   ╚═╝   "
    ]