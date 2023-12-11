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
import Lens.Micro ((^.))
import Data.IORef (newIORef, IORef, readIORef)

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
    | (label, color) <- [("pivot", red), ("highlight", cyan), ("temp", magenta), ("done", green)]]

initialState :: IO ApplicationState
initialState = do
  gen <- initStdGen
  tdref <- newIORef 15
  pure $ State {
            _randGen = gen
          , _sort = test
          , _tickDelay = tdref
          }

test :: Sort
test = let (x :| xs) = mergesort [1..150]
  in Sort (ListZipper [] x xs) True False

main :: IO ()
main = do
  let app = application
  eventChan <- newBChan 1
  st <- initialState
  _ <- forkIO (tickThread (st^.tickDelay) eventChan)
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  _ <- customMain initialVty buildVty (Just eventChan) app st
  pure ()

tickThread :: IORef Int -> BChan Tick -> IO ()
tickThread td chan = do
  writeBChan chan ()
  val <- readIORef td
  threadDelay (val * 1000)
  tickThread td chan
