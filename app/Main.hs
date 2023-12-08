module Main (main) where

import Control.Concurrent

import Graphics.Vty
import Graphics.Vty.CrossPlatform
import Brick.BChan (newBChan, BChan, writeBChan)

import Types
import UI (draw)
import Control (handleEvent)
import Brick (App (..), showFirstCursor, attrMap, attrName, fg, AttrName, customMain)
import System.Random (randomRIO)
import Control.Monad
import Algorithms.Quicksort (quicksort)
import Algorithms.SelectionSort (selectionsort)
import Algorithms.MergeSort (mergesort)

application :: App ApplicationState Tick Resource
application =
  App { appDraw = draw
      , appChooseCursor = showFirstCursor
      , appHandleEvent  = handleEvent onTick
      , appStartEvent   = pure ()
      , appAttrMap = const (attrMap defAttr colors)
      }

onTick :: ApplicationState -> ApplicationState
onTick st' = State $ map foo (sorts st')
  where foo st | finished st = st
               | []       <- rest st = st { finished = True }
               | (x : xs) <- rest st = st { current = x, rest = xs }

colors :: [(AttrName, Attr)]
colors = [(attrName label, fg color)
    | (label, color) <- [("pivot", blue), ("highlight", green)]]

initialState :: IO ApplicationState
initialState = do
  rand <- randoms 50 100
  let sorts' = [mergesort, quicksort, selectionsort]
  pure $ State $ map (initSort . ($ rand)) sorts'

initSort :: [[SortValue]] -> Sort
initSort [] = error "empty sort"
initSort (x : xs) = Sort x xs False

randoms :: Int -> Int -> IO [Int]
randoms n m = replicateM m (randomRIO (1, n))

main :: IO ()
main = do
  let app = application
  eventChan <- newBChan 10
  x <- forkIO (tickThread eventChan)
  print x
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  st <- initialState
  _ <- customMain initialVty buildVty (Just eventChan) app st
  pure ()

tickThread :: BChan Tick -> IO ()
tickThread chan = do
  writeBChan chan ()
  threadDelay 25000
  tickThread chan
