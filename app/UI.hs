module UI (draw) where

import Types (SortValue (..), ApplicationState (sorts), Sort (current))
import Brick
import Brick.Widgets.Center

draw :: ApplicationState -> [Widget ()]
draw st = [ vBox $ map (bars . current) (sorts st) ]

bars :: [SortValue] -> Widget ()
bars = hBox . map (\x -> bar (highlight x) (value x))

bar :: Maybe String -> Int -> Widget n
bar mark n
  | Just highlight' <- mark = withAttr (attrName highlight') bar'
  | otherwise   = bar'
  where bar' = padTop Max . vertical $ topBlock (n `rem` 8) : replicate (n `div` 8) '█'

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
