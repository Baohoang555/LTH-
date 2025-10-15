module Main where

import Network.Server (runServer)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runServer defaultPort
    [portStr] -> 
      case readMaybe portStr of
        Just port -> runServer port
        Nothing -> usage
    ["--help"] -> usage
    ["-h"] -> usage
    _ -> usage
  where
    defaultPort = 9160
    
    usage :: IO ()
    usage = do
      progName <- getProgName
      hPutStrLn stderr $ unlines
        [ "Connect Four Server"
        , ""
        , "Usage: " ++ progName ++ " [PORT]"
        , ""
        , "Arguments:"
        , "  PORT    Port number to listen on (default: 9160)"
        , ""
        , "Options:"
        , "  -h, --help    Show this help message"
        , ""
        , "Examples:"
        , "  " ++ progName ++ "           # Start on default port 9160"
        , "  " ++ progName ++ " 8080      # Start on port 8080"
        ]
      exitFailure