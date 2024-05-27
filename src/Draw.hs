module Draw (draw, drawList) where

import Xdg
import State
import qualified Graphics.X11.Xlib as X
import Config (background, foreground, fontSize, height)

draw :: X.Display -> X.Screen -> X.Window -> X.GC -> WindowState -> IO ()
draw display screen window gc state = do
    X.setBackground display gc background
    X.setForeground display gc foreground
    X.fillRectangle display window gc 0 0 (X.widthOfScreen screen) height
    X.drawImageString display window gc 200 fontSize (search state)
    drawList display screen window gc $ take 5 $ filterEntries (entries state) (search state)
    X.flush display

drawList :: X.Display -> X.Screen -> X.Window -> X.GC -> [DesktopEntry] -> IO ()
drawList display _ window gc xe = mapM_ (\(i, fe) -> do
    X.drawImageString display window gc 200 (fontSize + (i * fontSize)) (name fe)
    ) (zip [1..] xe)

