module Main where

import Network.Client (runClient)

main :: IO ()
main = do
  putStrLn "=== CONNECT FOUR CLIENT ==="
  putStrLn "Ket noi toi server..."
  -- Kết nối tới localhost:3000
  runClient "127.0.0.1" "3000"