module Control (handleEvent, getKeymap) where

import Graphics.Vty (Event (EvKey), Key (KEsc, KChar, KLeft, KRight), Modifier)
import Brick.Types (BrickEvent (VtyEvent, AppEvent), EventM)
import Brick (halt, get)

import Types
import Lens.Micro.Mtl ((%=), use, (.=))
import ListZipper (right, left, ListZipper (ListZipper), pointer)
import Lens.Micro ((^.))
import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.Bits (Bits(xor))
import Lens.Micro.Extras (view)
import Algorithms.Quicksort (quicksort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Algorithms.Shuffle (shuffleList)
import Algorithms.MergeSort (mergesort)

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

-- TODO
getKeymap :: ApplicationState -> [(Key, String, EventM Resource ApplicationState ())]
getKeymap st =
  [ (KEsc, "Quit", halt)
  , (KChar 'p', pauseDesc, paused %= not)
  , (KChar 'q', "Quicksort", startqs quicksort)
  , (KChar 'm', "Mergesort", startqs mergesort)
  , (KChar 's', "Shuffle", startqs (shuffleList (view randGen st)))
  ] ++ conditional isPaused
  [ (KRight, "forward", move right)
  , (KLeft, "backward", move left)
  ]
  where isPaused = st ^. paused
        pauseDesc | isPaused  = "Unpause"
                  | otherwise = "Pause"

startqs :: ([Int] -> NonEmpty [SortValue]) -> EventM Resource ApplicationState ()
startqs f = do
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
