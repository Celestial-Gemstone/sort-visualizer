module Algorithms.MergeSort (mergesort, ms, MsTree(..)) where
import Types (SortValue (SortValue))
import Data.List.NonEmpty (NonEmpty ((:|)))

data MsTree a = MsReduced [a] | MsUnreduced (MsTree a) [a] (MsTree a) deriving Show

mergesort :: [Int] -> NonEmpty [SortValue]
mergesort xs = let x = distribute xs
  in collect x :| iterateMs x

distribute :: [Int] -> MsTree Int
distribute []  = MsReduced []
distribute [x] = MsReduced [x]
distribute xs  = MsUnreduced (distribute l) [] (distribute r)
  where (l, r) = splitAt (length xs `div` 2) xs

iterateMs :: MsTree Int -> [[SortValue]]
iterateMs tree = rest
  where (x, changed) = ms tree
        rest | changed   = collect x : iterateMs x
             | otherwise = []

collect :: MsTree Int -> [SortValue]
collect = fst . collect' (Just "highlight")

collect' :: Maybe String -> MsTree Int -> ([SortValue], Bool)
collect' hl (MsReduced xs) = (map (`SortValue` hl) xs, True)
collect' hl (MsUnreduced l x r) = (l' ++ map (`SortValue` Just "temp") x ++ r', hldl || hldr)
  where (l', hldl) = collect' hl l
        (r', hldr) | hldl = collect' Nothing r
                   | otherwise = collect' hl r

ms :: MsTree Int -> (MsTree Int, Bool)
ms r@(MsReduced _) = (r, False)
ms (MsUnreduced (MsReduced l) acc (MsReduced r)) = (mergeStep l acc r, True)
-- ms (MsUnreduced l'@(MsReduced _) x r') = MsUnreduced l' x (ms r')
-- ms (MsUnreduced l' x r'@(MsReduced _)) = MsUnreduced (ms l') x r'
ms (MsUnreduced l x r) = (MsUnreduced l' x r', ls || rs)
  where (l', ls) = ms l
        (r', rs) | ls = (r, ls)
                 | otherwise = ms r

mergeStep :: [Int] -> [Int] -> [Int] -> MsTree Int
mergeStep [] acc [] = MsReduced acc
mergeStep (x : xs) acc [] = MsUnreduced (MsReduced xs) (acc ++ [x]) (MsReduced [])
mergeStep [] acc (y : ys) = MsUnreduced (MsReduced []) (acc ++ [y]) (MsReduced ys)
mergeStep xs'@(x : xs) acc ys'@(y : ys)
  | x < y     = MsUnreduced (MsReduced xs) (acc ++ [x]) (MsReduced ys')
  | otherwise = MsUnreduced (MsReduced xs') (acc ++ [y]) (MsReduced ys)

