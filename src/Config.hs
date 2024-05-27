module Config (background, foreground, height, fontSize, fontFace) where

import qualified Graphics.X11 as X
import Data.Word (Word32)
import GHC.Int (Int32)

background :: X.Pixel
background = 100

foreground :: X.Pixel
foreground = 150

height :: Word32
height = 200

fontSize :: Int32
fontSize = 13

fontFace :: String
fontFace = "-misc-fixed-*-*-*-*-13-*-*-*-*-*-*-*"

