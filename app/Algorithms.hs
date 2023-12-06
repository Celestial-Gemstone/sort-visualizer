module Algorithms (SortData(..), QuickSort, SortValue(..), qsData, quicksort) where

import Data.List (partition)
import Types


quicksort :: QsData Int -> QuickSort
quicksort x = let (x', done') = qs x
  in SortData foo x' done'

highlightColor :: Bool -> String
highlightColor pivot
  | pivot     = "pivot"
  | otherwise = "highlight"

qs :: Ord a => QsData a -> (QsData a, Bool)
qs l@(Leaf []) = (l, False)
qs (Leaf (x : xs)) = (qs' x xs, True)
qs (Reduced _ l x r)
  | lSuccess  = (Reduced False l' x r,  lSuccess)
  | otherwise = (Reduced False l' x r', rSuccess)
  where (l', lSuccess) = qs l
        (r', rSuccess) = qs r

qs' :: Ord a => a -> [a] -> QsData a
qs' x xs = Reduced True (Leaf l) x (Leaf r)
  where (l, r) = partition (< x) xs

foo :: QsData Int -> [SortValue]
foo (Leaf xs) = map (`SortValue` Nothing) xs
foo (Reduced b l x r) = foo l ++ [SortValue x (Just $ highlightColor b)] ++ foo r

qsData = SortData
  { order = foo
  , sortData = Leaf nums
  , done = False }

nums :: [Int]
nums = [193,105,18,133,62,16,164,190,52,154,160,91,107,45,101,66,33,19,28,145,44,123,94,129,32,186,185,87,51,43,47,200,196,89,12,95,166,7,192,147,73,144,171,92,180,194,30,55,99,23,163,170,54,13,183,82,174,198,15,177,72,100,48,41,81,29,130,184,85,167,134,109,126,9,158,173,6,63,115,128,118,74,17,26,172,150,37,68,176,108,35,113,11,46,4,179,93,80,162,36,27,88,124,53,110,58,69,8,131,34,76,143,197,116,142,169,168,3,136,122,135,153,138,112,25,5,2,149,10,152,182,71,195,75,146,181,199,77,191,31,96,56,70,61,60,137,159,98,79,157,104,64,165,187,161,103,22,132,178,78,38,189,155,106,121,1,141,111,120,21,49,24,148,40,139,117,175,127,114,156,86,97,67,42,65,188,140,59,90,102,20,151,119,83,14,84,50,39,57,125]
