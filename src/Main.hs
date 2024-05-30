module Main (main) where

import Xdg
import Draw
import State
import Config
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as X
import Data.Bits
import Graphics.X11 (KeySym)
import System.Exit (exitSuccess)
import System.Directory.Internal.Prelude (getArgs)

main :: IO ()
main = do
    args <- parse <$> getArgs
    display <- X.openDisplay ""
    xdgEntries <- scanEntries
    let state = WindowState { search="", entries=xdgEntries, exitWith=Nothing }
    let screen = X.defaultScreenOfDisplay display

    window <- X.allocaSetWindowAttributes $ \wa -> do
        rw <- X.rootWindow display 0
        X.set_override_redirect wa True
        X.set_background_pixel wa 0
        X.createWindow display rw 0 0 (fromIntegral (X.displayWidth display (X.screenNumberOfScreen screen))) height
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
    loop display screen window gc font args state

    X.freeGC display gc
    X.freeFont display font

loop :: X.Display -> X.Screen -> X.Window -> X.GC -> X.FontStruct -> Args -> WindowState -> IO ()
loop display screen window gc font args state =
    X.allocaXEvent $ \e -> do
        X.nextEvent display e
        next <- handleEvent state e
        draw display screen window gc font args next
        case exitWith next of
            Just ec -> ec
            Nothing -> loop display screen window gc font args next

handleEvent :: WindowState -> X.XEventPtr -> IO WindowState
handleEvent s e = do
    typ <- X.get_EventType e
    case typ of
        _ | typ == X.keyPress -> do
            buf <- X.lookupString (X.asKeyEvent e)
            return $ handleKey s buf
        _ -> do
            return s

handleKey :: WindowState -> (Maybe KeySym, String) -> WindowState
handleKey s (Just sym, ks)
    | sym == X.xK_BackSpace =
        if null (search s)
        then s { exitWith = Just exitSuccess }
        else s { search = init $ search s }
    | sym >= X.xK_space && sym <= X.xK_ydiaeresis = s { search = search s ++ ks }
    | sym == X.xK_Escape = s { exitWith = Just exitSuccess }
    | sym == X.xK_KP_Enter || sym == X.xK_Return = s { exitWith = Just $ launchFirst s }
    | otherwise = s
handleKey s (Nothing, _) = s

