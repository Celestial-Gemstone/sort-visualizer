module Algorithms.Bogosort (bogosort) where

import Data.List.NonEmpty (NonEmpty, last)
import Types (SortValue, value)
import System.Random (RandomGen)
import Algorithms.Shuffle (shuffleList)
import Lens.Micro.Extras (view)

bogosort :: RandomGen g => g -> [Int] -> NonEmpty [SortValue]
bogosort g xs = y'
  where y = shuffleList g xs
        vals = map (view value) (Data.List.NonEmpty.last y)
        y' | isSorted vals = y
           | otherwise     = y <> bogosort g vals

isSorted :: Ord a => [a] -> Bool
isSorted []  = True
isSorted [_] = True
isSorted (x : y : xs) = x <= y && isSorted (y : xs)
