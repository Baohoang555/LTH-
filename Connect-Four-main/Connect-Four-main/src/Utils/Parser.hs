module Utils.Parser (parseColumn) where

import Text.Read (readMaybe)

-- Parse input cột từ string
parseColumn :: String -> Either String Int
parseColumn input = 
  case readMaybe input of
    Nothing -> Left "Vui long nhap so!"
    Just col 
      | col >= 0 && col <= 6 -> Right col
      | otherwise -> Left "Cot phai tu 0-6!"