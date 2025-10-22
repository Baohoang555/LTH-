-- Attributes.hs (adapt màu cho red/black/blue board)
module UI.Attributes where

import qualified Brick.AttrMap as A
import Brick.Util (fg, bg)
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V

primaryAttr :: A.AttrName
primaryAttr = A.attrName "primary"

primaryColor :: V.Color
primaryColor = V.rgbColor 0 0 255  -- Blue bg for board

secondaryAttr :: A.AttrName
secondaryAttr = A.attrName "secondary"

secondaryColor :: V.Color
secondaryColor = V.rgbColor 128 128 128  -- Gray for text

redAttr :: A.AttrName
redAttr = A.attrName "red"

blackAttr :: A.AttrName
blackAttr = A.attrName "black"

winAttr :: A.AttrName
winAttr = A.attrName "win"  -- For highlight winning line

errorAttr :: A.AttrName
errorAttr = A.attrName "error"

attributeMap :: A.AttrMap
attributeMap =
  A.attrMap
    V.defAttr
    [ (primaryAttr, bg primaryColor)
    , (secondaryAttr, fg secondaryColor)
    , (L.listSelectedAttr, fg secondaryColor `V.withStyle` V.bold)
    , (redAttr, fg V.red)
    , (blackAttr, fg V.black)
    , (winAttr, fg V.green `V.withStyle` V.bold)
    , (errorAttr, fg V.red)
    ]