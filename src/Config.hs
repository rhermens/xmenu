module Config (height, fontSize, fontFace, Args (..), parse) where

import Data.Word (Word32)
import GHC.Int (Int32)
import GHC.Word (Word64)
import Data.List (isPrefixOf, find)
import Numeric (readHex)

height :: Word32
height = fromIntegral fontSize * 2

fontSize :: Int32
fontSize = 13

fontFace :: String
fontFace = "-misc-fixed-*-*-*-*-13-*-*-*-*-*-*-*"

data Args = Args {
    bg :: Word64,
    fg :: Word64
} deriving (Show)

parse :: [String] -> Args
parse args = Args {
    bg=maybe 0x000000 (fst . head . readHex . drop 6) (find (isPrefixOf "--bg=#") args),
    fg=maybe 0x000000 (fst . head . readHex . drop 6) (find (isPrefixOf "--fg=#") args)
}

