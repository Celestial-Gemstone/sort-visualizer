module Algorithms.SelectionSort (selectionsort) where

import Types
import Data.List (delete)

selectionsort :: [Int] -> [[SortValue]]
selectionsort xs = iterateSelSort (SelSortData xs [])

iterateSelSort :: SelSortData -> [[SortValue]]
iterateSelSort l@(SelSortData [] _) = [collect l]
iterateSelSort l = collect l : iterateSelSort (selSort l)

collect :: SelSortData -> [SortValue]
collect (SelSortData rs (x : xs)) = values rs Nothing ++ [SortValue x (Just "pivot")] ++ values xs (Just "highlight")
collect (SelSortData rs []) = values rs Nothing

values :: [Int] -> Maybe String -> [SortValue]
values xs label = map (`SortValue` label) xs

selSort :: SelSortData -> SelSortData
selSort (SelSortData rs sorted) = SelSortData (delete x rs) (x : sorted)
  where x = maximum rs

data SelSortData = SelSortData [Int] [Int]
