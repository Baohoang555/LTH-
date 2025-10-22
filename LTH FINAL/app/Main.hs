module Main where

import Brick.Main (defaultMain)
import UI.Offline (localApp, initialGame)  -- For offline
import UI.Online (onlineApp)  -- For online, but need connection

main :: IO ()
main = do
  _ <- defaultMain localApp initialGame  -- Run offline UI
  -- For online: Need WS connection from Client.hs, e.g., runClient "YourName" >> defaultMain onlineApp someState
  putStrLn "Game ended"