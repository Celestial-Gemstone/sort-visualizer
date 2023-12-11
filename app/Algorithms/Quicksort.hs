module Algorithms.Quicksort (quicksort) where

import Types
import Data.List.NonEmpty (NonEmpty ((:|)))

data QsTree a
  = Unreduced [a]
  | Reduced Bool (QsTree a) a (QsTree a)
  | Reducing [a] (a, [a]) [a]
  deriving Show

quicksort :: [Int] -> NonEmpty [SortValue]
quicksort xs = let ur = Unreduced xs
  in collect ur :| quicksort' ur

quicksort' :: QsTree Int -> [[SortValue]]
quicksort' tree
  | change    = collect rec : quicksort' rec
  | otherwise = []
  where (rec, change) = qs tree

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
qs (Unreduced (x : xs)) = (Reducing [] (x, xs) [], True)
qs (Reducing l x r) = (reduce l x r, True)
qs (Reduced _ l x r) = (Reduced False ln x r', lSuccess || rSuccess)
  where (l', lSuccess) = qs l
        (r', rSuccess) = qs r
        ln | rSuccess  = l
           | otherwise = l'

reduce :: Ord a => [a] -> (a, [a]) -> [a] -> QsTree a
reduce l (x, []) r = Reduced True (Unreduced l) x (Unreduced r)
reduce l (x, y : xs) r = Reducing l' (x, xs) r'
  where (l', r') | y < x     = (y : l, r)
                 | otherwise = (l, y : r)

collect :: QsTree Int -> [SortValue]
collect (Reducing l (p, ys) r) = concat [collectValues Nothing l, [SortValue p (Just "pivot")], collectValues (Just "temp") ys, collectValues Nothing r]
collect (Unreduced xs) = map (`SortValue` Nothing) xs
collect (Reduced b l x r) = concat [collect l, [SortValue x hl], collect r]
  where hl = Just (highlightLabel b)

collectValues :: Maybe String -> [Int] -> [SortValue]
collectValues hl = map (`SortValue` hl)
