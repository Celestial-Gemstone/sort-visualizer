module ListZipper where

data ListZipper a = ListZipper [a] a [a]

left :: ListZipper a -> Maybe (ListZipper a)
left (ListZipper [] _ _) = Nothing
left (ListZipper (l : ls) x rs) = Just $ ListZipper ls l (x : rs)

right :: ListZipper a -> Maybe (ListZipper a)
right (ListZipper _ _ []) = Nothing
right (ListZipper ls x (r : rs)) = Just $ ListZipper (x : ls) r rs

pointer :: ListZipper a -> a
pointer (ListZipper _ x _) = x
