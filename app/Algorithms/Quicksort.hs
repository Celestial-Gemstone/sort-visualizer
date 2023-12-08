module Algorithms.Quicksort (quicksort) where

import Data.List (partition)
import Types

quicksort :: [Int] -> [[SortValue]]
quicksort = map collect . iterateQs . Unreduced

iterateQs :: Ord a => QsTree a -> [QsTree a]
iterateQs tree = tree : foo rec
  where (rec, change) = qs tree
        foo | change    = iterateQs
            | otherwise = pure

highlightLabel :: Bool -> String
highlightLabel pivot
  | pivot     = "pivot"
  | otherwise = "highlight"

qs :: Ord a => QsTree a -> (QsTree a, Bool)
qs l@(Unreduced []) = (l, False)
qs (Unreduced (x : xs)) = (reduce x xs, True)
qs (Reduced _ l x r) = (Reduced False ln x r', lSuccess || rSuccess)
  where (l', lSuccess) = qs l
        (r', rSuccess) = qs r
        ln | rSuccess  = l
           | otherwise = l'

reduce :: Ord a => a -> [a] -> QsTree a
reduce x xs = Reduced True (Unreduced l) x (Unreduced r)
  where (l, r) = partition (< x) xs

collect :: QsTree Int -> [SortValue]
collect (Unreduced xs) = map (`SortValue` Nothing) xs
collect (Reduced b l x r) = concat [collect l, [SortValue x hl], collect r]
  where hl = Just (highlightLabel b)
