module Xdg (DesktopEntry (..), scanEntries) where

import System.Directory
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.List.Split (splitOn)
import Data.Text (replace, pack, unpack, strip)

data DesktopEntry = DesktopEntry {
    name :: String,
    exec :: String
} deriving (Show)

xdgDataDirectory :: String
xdgDataDirectory = "/usr/share/applications/"

execVars :: [String]
execVars = ["%d","%D","%n","%N","%v","%m","%f","%F","%u","%U","%i","%c","%k"]

scanEntries :: IO [DesktopEntry]
scanEntries = do
    files <- listDirectory xdgDataDirectory
    contents <- mapM (readFile . (xdgDataDirectory ++)) files
    return $ mapMaybe fromFileContent contents

fromFileContent :: String -> Maybe DesktopEntry
fromFileContent s = do
    case (lookup "Name" cfg, lookup "Exec" cfg) of
        (Just n, Just e) -> Just DesktopEntry {name=n, exec=substituteExecVars e}
        _ -> Nothing
    where
        lns = filter (\l -> not ("#" `isPrefixOf` l && null l)) (lines s)
        cfg = mapMaybe (\l -> case splitOn "=" l of
                [k, v] -> Just (k, v)
                _ -> Nothing
                ) lns

substituteExecVars :: String -> String
substituteExecVars s =
    unpack . strip $ foldl (\acc var -> replace (pack var) (pack "") acc) (pack s) execVars

