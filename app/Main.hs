module Main (main) where

import Control.Concurrent

import Graphics.Vty
import Graphics.Vty.CrossPlatform
import Brick.BChan (newBChan, BChan, writeBChan)

import Types
import UI (draw)
import Control (handleEvent)
import Brick (App (..), showFirstCursor, attrMap, attrName, fg, AttrName, customMain)
import System.Random (initStdGen)
import ListZipper (ListZipper(ListZipper))
import Data.List.NonEmpty (NonEmpty((:|)))
import Algorithms.MergeSort (mergesort)

application :: App ApplicationState Tick Resource
application =
  App { appDraw = draw
      , appChooseCursor = showFirstCursor
      , appHandleEvent  = handleEvent
      , appStartEvent   = pure ()
      , appAttrMap = const (attrMap defAttr colors)
      }

colors :: [(AttrName, Attr)]
colors = [(attrName label, fg color)
    | (label, color) <- [("pivot", red), ("highlight", cyan), ("temp", magenta)]]

initialState :: IO ApplicationState
initialState = State test True False <$> initStdGen

test = let (x :| xs) = mergesort [1..150]
  in Sort $ ListZipper [] x xs

main :: IO ()
main = do
  let app = application
  eventChan <- newBChan 1
  x <- forkIO (tickThread 10 eventChan)
  print x
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  st <- initialState
  _ <- customMain initialVty buildVty (Just eventChan) app st
  pure ()

tickThread :: Int -> BChan Tick -> IO ()
tickThread delay chan = do
  writeBChan chan ()
  threadDelay (delay * 1000)
  tickThread delay chan
