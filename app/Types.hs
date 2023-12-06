module Types where

newtype ApplicationState =
  State {
      all :: QuickSort
  }

type Resource = ()
type Tick = ()

data SortData a = SortData {
    order :: a -> [SortValue],
    sortData :: a,
    done :: Bool
    }

data SortValue = SortValue {
    value :: Int,
    highlight :: Maybe String
    }

data QsData a = Leaf [a] | Reduced Bool (QsData a) a (QsData a)
  deriving Show

type QuickSort = SortData (QsData Int)
