module Main where

import Network.Server (runServer)

main :: IO ()
main = do
  putStrLn "=== CONNECT FOUR SERVER ==="
  runServer "3000"