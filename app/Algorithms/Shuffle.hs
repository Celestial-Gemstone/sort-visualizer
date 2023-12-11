module Algorithms.Shuffle (shuffleList) where

import System.Random (RandomGen, uniformR)
import Data.List (delete)
import Data.List.NonEmpty(NonEmpty(..))
import Types (SortValue (SortValue))


data ShuffleData a = ShuffleData [a] [a]

shuffleList :: RandomGen g => g -> [Int] -> NonEmpty [SortValue]
shuffleList g xs = let x = ShuffleData [] xs
  in collect x :| shuffle g x

shuffle :: RandomGen g => g -> ShuffleData Int -> [[SortValue]]
shuffle g d = collect d : rec
  where (r, g', done) = shuffleStep g d
        rec | done = []
            | otherwise = shuffle g' r

collect :: ShuffleData Int -> [SortValue]
collect (ShuffleData xs ys) = collectValues Nothing xs ++ collectValues (Just "highlight") ys

collectValues :: Maybe String -> [Int] -> [SortValue]
collectValues hl = map (`SortValue` hl)

shuffleStep :: (Eq a, RandomGen b) => b -> ShuffleData a -> (ShuffleData a, b, Bool)
shuffleStep g d@(ShuffleData _ []) = (d, g, True)
shuffleStep g (ShuffleData xs ys)  = (ShuffleData (x : xs) (delete x ys), g', False)
  where x = ys !! ix
        (ix, g') = uniformR (0, length ys - 1) g
