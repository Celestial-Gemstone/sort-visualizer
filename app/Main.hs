module Main (main) where

import Control.Concurrent

import Graphics.Vty
import Brick.BChan (newBChan, BChan, writeBChan)

import Algorithms (qsData, quicksort)
import Types
import UI (draw)
import Control (handleEvent)
import Brick (App (..), showFirstCursor, attrMap, attrName, fg, AttrName, customMain)

sort :: ApplicationState -> ApplicationState
sort (State st) = State $ quicksort (sortData st)

application :: App ApplicationState Tick Resource
application =
  App { appDraw = draw
      , appChooseCursor = showFirstCursor
      , appHandleEvent  = handleEvent sort
      , appStartEvent   = pure ()
      , appAttrMap = const (attrMap defAttr colors)
      }

colors :: [(AttrName, Attr)]
colors = [(attrName label, fg color)
    | (label, color) <- [("pivot", red), ("highlight", blue)]]

initialState :: ApplicationState
initialState = State qsData

main :: IO ()
main = do
  let app = application
  eventChan <- newBChan 10
  x <- forkIO (tickThread eventChan)
  print x
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  _ <- customMain initialVty buildVty (Just eventChan) app initialState
  pure ()

tickThread :: BChan Tick -> IO ()
tickThread chan = do
  writeBChan chan ()
  threadDelay 30000
  tickThread chan
