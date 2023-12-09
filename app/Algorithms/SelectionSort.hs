module Algorithms.SelectionSort (selectionsort) where

import Types
import Data.List (delete)
import Data.List.NonEmpty (NonEmpty (..))

selectionsort :: [Int] -> NonEmpty [SortValue]
selectionsort xs = let d = SelSortData xs []
  in collect d :| iterateSelSort (selSort d)

iterateSelSort :: SelSortData -> [[SortValue]]
iterateSelSort l@(SelSortData [] _) = [collect l]
iterateSelSort l = collect l : iterateSelSort (selSort l)

collect :: SelSortData -> [SortValue]
collect (SelSortData rs (x : xs)) = collectValues rs Nothing ++ [SortValue x (Just "pivot")] ++ collectValues xs (Just "highlight")
collect (SelSortData rs []) = collectValues rs Nothing

collectValues :: [Int] -> Maybe String -> [SortValue]
collectValues xs label = map (`SortValue` label) xs

selSort :: SelSortData -> SelSortData
selSort (SelSortData rs sorted) = SelSortData (delete x rs) (x : sorted)
  where x = maximum rs

data SelSortData = SelSortData [Int] [Int]
