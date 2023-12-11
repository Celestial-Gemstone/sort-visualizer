module Control (handleEvent, getKeymap) where

import Graphics.Vty (Event (EvKey), Key (KEsc, KChar, KLeft, KRight), Modifier)
import Brick.Types (BrickEvent (VtyEvent, AppEvent), EventM)
import Brick (halt, get)

import Types
import Lens.Micro.Mtl ((%=), use, (.=), (+=), (-=))
import ListZipper (right, left, ListZipper (ListZipper), pointer)
import Lens.Micro ((^.))
import Control.Monad (unless)
import Data.Foldable (traverse_)
import Lens.Micro.Extras (view)
import Algorithms.Quicksort (quicksort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Algorithms.Shuffle (shuffleList)
import Algorithms.MergeSort (mergesort)
import Algorithms.SelectionSort (selectionsort)
import Algorithms.Bogosort (bogosort)
import Data.IORef (atomicModifyIORef)
import Control.Monad.IO.Class (MonadIO(..))

handleEvent :: BrickEvent Resource Tick -> EventM Resource ApplicationState ()
handleEvent ev
  | VtyEvent ev' <- ev = handleVtyEvent ev'
  | AppEvent ()  <- ev = flip unless (move right) =<< use paused
  | otherwise = pure ()

handleVtyEvent :: Event -> EventM Resource ApplicationState ()
handleVtyEvent (EvKey key mods) = handleKey key mods
handleVtyEvent _ = pure ()

handleKey :: Key -> [Modifier] -> EventM Resource ApplicationState ()
handleKey key _ = do
  foo <- get
  let m = getKeymap foo
  let x = filter (\(k, _, _) -> k == key) m
  traverse_ (\(_, _, ev) -> ev) x

move :: (ListZipper [SortValue] -> Maybe (ListZipper [SortValue])) -> EventM Resource ApplicationState ()
move f = do
  sortValues <- use sort
  apply $ f (sortValues ^. values)

apply :: Maybe (ListZipper [SortValue]) -> EventM Resource ApplicationState ()
apply (Just z) = sort.values .= z
apply _        = done .= True

getKeymap :: ApplicationState -> [(Key, String, EventM Resource ApplicationState ())]
getKeymap st =
  [ (KEsc, "Quit", halt)
  , (KChar '+', "Increase tick delay", ggg (+10))
  , (KChar '-', "Decrease tick delay", ggg (\x -> max 0 (x - 10)))
  ] ++ conditional (not isDone)
  [ (KChar ' ', pauseDesc, paused %= not)
  ] ++ conditional isPaused
  [ (KRight, "forward", move right)
  , (KLeft, "backward", move left)
  ] ++ conditional (isPaused || isDone)
  [ (KChar 'q', "Quicksort", startAlgorithm quicksort)
  , (KChar 'm', "Mergesort", startAlgorithm mergesort)
  , (KChar 'c', "Selectionsort", startAlgorithm selectionsort)
  , (KChar 'b', "Bogosort", startAlgorithm bogosort)
  , (KChar 's', "Shuffle", startAlgorithm (shuffleList (view randGen st)))
  ]
  where isPaused = st ^. paused
        isDone = st ^. done
        pauseDesc | isPaused  = "Unpause"
                  | otherwise = "Pause"

ggg :: (Int -> Int) -> EventM Resource ApplicationState ()
ggg f = do
  td <- use tickDelay
  liftIO $ atomicModifyIORef td (\x -> (f x, ()))

startAlgorithm :: ([Int] -> NonEmpty [SortValue]) -> EventM Resource ApplicationState ()
startAlgorithm f = do
  done   .= False
  paused .= False
  sort.values %= useAlgorithm f

useAlgorithm :: ([Int] -> NonEmpty [SortValue]) -> ListZipper [SortValue] -> ListZipper [SortValue]
useAlgorithm f x = let vals = view value <$> pointer x
          in prepare $ f vals

prepare :: NonEmpty [SortValue] -> ListZipper [SortValue]
prepare (x :| xs) = ListZipper [] x xs


conditional :: Bool -> [a] -> [a]
conditional bool xs
  | bool = xs
  | otherwise = []
