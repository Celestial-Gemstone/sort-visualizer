module Algorithms.MergeSort (mergesort, ms, MsTree(..)) where
import Types (SortValue (SortValue), QsTree (Reduced))

mergesort :: [Int] -> [[SortValue]]
mergesort = iterateMs . distribute

distribute :: [Int] -> MsTree Int
distribute []  = MsReduced []
distribute [x] = MsReduced [x]
distribute xs  = MsUnreduced (distribute l) [] (distribute r)
  where (l, r) = splitAt (length xs `div` 2) xs

iterateMs :: MsTree Int -> [[SortValue]]
iterateMs tree = collect tree : iterateMs (ms tree)

collect :: MsTree Int -> [SortValue]
collect (MsReduced xs) = map (`SortValue` Just "highlight") xs
collect (MsUnreduced l x r) = collect l ++ map (`SortValue` Just "pivot") x ++ collect r

merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge xs'@(x : xs) ys'@(y : ys)
  | x < y     = x : merge xs ys'
  | otherwise = y : merge xs' ys

data MsTree a = MsReduced [a] | MsUnreduced (MsTree a) [a] (MsTree a) deriving Show

ms :: MsTree Int -> MsTree Int
ms (MsUnreduced (MsReduced l) acc (MsReduced r)) = mergeStep l acc r
ms (MsUnreduced l'@(MsReduced _) x r') = MsUnreduced l' x (ms r')
ms (MsUnreduced l' x r'@(MsReduced _)) = MsUnreduced (ms l') x r'
ms (MsUnreduced l' x r') = MsUnreduced l' x (ms r')
ms r@(MsReduced _) = r

mergeStep :: [Int] -> [Int] -> [Int] -> MsTree Int
mergeStep [] acc [] = MsReduced acc
mergeStep (x : xs) acc [] = MsUnreduced (MsReduced xs) (acc ++ [x]) (MsReduced [])
mergeStep [] acc (y : ys) = MsUnreduced (MsReduced []) (acc ++ [y]) (MsReduced ys)
mergeStep xs'@(x : xs) acc ys'@(y : ys)
  | x < y     = MsUnreduced (MsReduced xs) (acc ++ [x]) (MsReduced ys')
  | otherwise = MsUnreduced (MsReduced xs') (acc ++ [y]) (MsReduced ys)



