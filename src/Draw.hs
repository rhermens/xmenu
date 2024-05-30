module Draw (draw, drawList) where

import Xdg
import State
import Config
import qualified Graphics.X11.Xlib as X

draw :: X.Display -> X.Screen -> X.Window -> X.GC -> X.FontStruct -> Args -> WindowState -> IO ()
draw display screen window gc font args state = do
    X.setForeground display gc (bg args)
    X.setBackground display gc (fg args)
    X.fillRectangle display window gc 0 0 (X.widthOfScreen screen) height

    X.setBackground display gc (bg args)
    X.setForeground display gc (fg args)
    X.drawImageString display window gc (textWidth `div` 2) textWidth (search state)
    drawList display screen window gc font (fromIntegral (X.widthOfScreen screen `div` 3)) textWidth filteredEntries
    X.flush display
    where
        textWidth = X.textWidth font "..."
        filteredEntries = take 5 $ filterEntries (entries state) (search state)


drawList :: X.Display -> X.Screen -> X.Window -> X.GC -> X.FontStruct -> X.Position -> X.Position -> [DesktopEntry] -> IO ()
drawList _ _ _ _ _ _ _ [] =
    return ()

drawList display screen window gc font pos y (x:xs) = do
    X.drawImageString display window gc pos y (name x)
    drawList display screen window gc font (pos + X.textWidth font (name x ++ "...")) y xs

