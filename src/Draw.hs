module Draw (draw, drawList) where

import Xdg
import State
import qualified Graphics.X11.Xlib as X
import Config (background, foreground, fontSize, height)

draw :: X.Display -> X.Screen -> X.Window -> X.GC -> X.FontStruct -> WindowState -> IO ()
draw display screen window gc font state = do
    let textWidth = X.textWidth font "..."
    X.setBackground display gc background
    X.setForeground display gc foreground
    X.fillRectangle display window gc 0 0 (X.widthOfScreen screen) height
    X.drawImageString display window gc (textWidth `div` 2) textWidth (search state)
    drawList display screen window gc font (fromIntegral (X.widthOfScreen screen `div` 3)) textWidth $ take 5 $ filterEntries (entries state) (search state)
    X.flush display

drawList :: X.Display -> X.Screen -> X.Window -> X.GC -> X.FontStruct -> X.Position -> X.Position -> [DesktopEntry] -> IO ()
drawList _ _ _ _ _ _ _ [] =
    return ()

drawList display screen window gc font pos y (x:xs) = do
    X.drawImageString display window gc pos y (name x)
    drawList display screen window gc font (pos + X.textWidth font (name x)) y xs

