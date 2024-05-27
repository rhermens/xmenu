module Main (main) where

import Xdg
import Config
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as X
import Data.Bits
import Graphics.X11 (KeySym)
import Control.Exception (handle, IOException)
import Data.Text (pack, toLower, isInfixOf)

data WindowState = WindowState {
    entries :: [DesktopEntry],
    search :: String
} deriving (Show)

filterEntries :: [DesktopEntry] -> String -> [DesktopEntry]
filterEntries xe s = filter (\e -> toLower (pack s) `isInfixOf` toLower (pack (name e))) xe

handleExc :: IOException -> IO ()
handleExc = print

main :: IO ()
main = handle handleExc $ do
    display <- X.openDisplay ""
    let screen = X.defaultScreenOfDisplay display
    rw <- X.rootWindow display (X.screenNumberOfScreen screen)

    window <- X.allocaSetWindowAttributes $ \wa -> do
        X.set_override_redirect wa True
        X.set_background_pixel wa 0
        X.createWindow display rw 0 0 (X.widthOfScreen screen) height
            X.copyFromParent X.copyFromParent
            X.inputOutput
            (X.defaultVisualOfScreen screen)
            (X.cWEventMask .|. X.cWOverrideRedirect .|. X.cWBackPixel .|. X.cWBackPixmap)
            wa

    gc <- X.createGC display window
    font <- X.loadQueryFont display fontFace
    X.selectInput display window (X.exposureMask .|. X.keyPressMask .|. X.visibilityChangeMask)

    X.mapWindow display window
    X.setInputFocus display window X.revertToParent X.currentTime
    xdgEntries <- scanEntries

    let state = WindowState {search="", entries=xdgEntries}
    loop display screen window gc state

    X.freeGC display gc
    X.freeFont display font

loop :: X.Display -> X.Screen -> X.Window -> X.GC -> WindowState -> IO ()
loop display screen window gc state = do
    X.allocaXEvent $ \e -> do
        X.nextEvent display e
        next <- handleEvent state e
        draw display screen window gc next
        X.flush display
        loop display screen window gc next

handleEvent :: WindowState -> X.XEventPtr -> IO WindowState
handleEvent s e = do
    typ <- X.get_EventType e
    case typ of
        _ | typ == X.keyPress -> do
            buf <- X.lookupString (X.asKeyEvent e)
            print buf
            print (search s)
            return $ handleKey s buf
        _ -> do
            print "other"
            return s

handleKey :: WindowState -> (Maybe KeySym, String) -> WindowState
handleKey s (Just sym, ks)
    | sym == X.xK_BackSpace = s {search = init $ search s}
    | sym >= X.xK_space && sym <= X.xK_ydiaeresis = s {search = search s ++ ks}
    | otherwise = s

handleKey s (Nothing, _) = s

draw :: X.Display -> X.Screen -> X.Window -> X.GC -> WindowState -> IO ()
draw display screen window gc state = do
    X.setBackground display gc background
    X.setForeground display gc foreground
    X.fillRectangle display window gc 0 0 (X.widthOfScreen screen) height
    X.drawImageString display window gc 200 fontSize (search state)
    drawList display screen window gc (take 5 (filterEntries (entries state) (search state)))

drawList :: X.Display -> X.Screen -> X.Window -> X.GC -> [DesktopEntry] -> IO ()
drawList display _ window gc xe = do
    mapM_ (\(i, fe) -> do
        print (name fe)
        X.drawImageString display window gc 200 (fontSize + (i * fontSize)) (name fe)
        ) (zip [1..] xe)

