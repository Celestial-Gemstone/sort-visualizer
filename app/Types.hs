module Types (ApplicationState(..), Sort(..), Resource, Tick, SortValue(..), QsTree(..)) where

data ApplicationState =
  State {
      sorts :: [Sort]
  }

data Sort = Sort {
      current  :: [SortValue],
      rest     :: [[SortValue]],
      finished :: Bool
}

type Resource = ()
type Tick = ()

data SortValue = SortValue {
    value :: Int,
    highlight :: Maybe String
    }

data QsTree a = Unreduced [a] | Reduced Bool (QsTree a) a (QsTree a)
  deriving Show
