module UI (draw) where

import Types
import Brick
import Brick.Widgets.Center
import Lens.Micro.Mtl (view)
import ListZipper (pointer)
import Control (getKeymap)
import Graphics.Vty (Key (KChar, KRight, KLeft))

draw :: ApplicationState -> [Widget ()]
draw st = [ center (drawBars st) <=> hBox (padLeftRight 1 . str <$> buildHelp st)]

buildHelp :: ApplicationState -> [String]
buildHelp st = let keymap = getKeymap st
  in map (\(key, desc, _) -> makeHelp (showKey key) desc) keymap

showKey :: Key -> String
showKey (KChar c) = [c]
showKey KRight = "→"
showKey KLeft  = "←"
showKey x = tail $ show x

makeHelp :: String -> String -> String
makeHelp key desc = concat [key, "(", desc, ")"]


drawBars :: ApplicationState -> Widget ()
drawBars = hBox . map drawBarHighlight . pointer . view (sort . values)

drawBarHighlight :: SortValue -> Widget n
drawBarHighlight val = highlightBar val $ padTop Max (drawBar (view value val))

highlightBar :: SortValue -> Widget n -> Widget n
highlightBar = maybe id (withAttr . attrName) . view highlight

drawBar :: Int -> Widget n
drawBar n = vertical $ topBlock (n `rem` 8) : replicate (n `div` 8) '█'

topBlock :: (Eq a, Num a) => a -> Char
topBlock 0 = ' '
topBlock 1 = '▁'
topBlock 2 = '▂'
topBlock 3 = '▃'
topBlock 4 = '▄'
topBlock 5 = '▅'
topBlock 6 = '▆'
topBlock 7 = '▇'
topBlock _ = undefined

vertical :: String -> Widget n
vertical = vBox . map (str . pure)
