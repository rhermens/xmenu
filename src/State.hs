module State (WindowState (..), filterEntries, launchFirst) where

import Xdg
import Data.Text (toLower, isInfixOf, pack)
import System.Process (spawnProcess)

data WindowState = WindowState {
    entries :: [DesktopEntry],
    search :: String,
    exitWith :: Maybe (IO ())
}

filterEntries :: [DesktopEntry] -> String -> [DesktopEntry]
filterEntries xe s = filter (\e -> toLower (pack s) `isInfixOf` toLower (pack (name e))) xe

launchFirst :: WindowState -> IO ()
launchFirst s = do
    _ <- spawnProcess (exec $ head $ filterEntries (entries s) (search s)) []
    return ()

