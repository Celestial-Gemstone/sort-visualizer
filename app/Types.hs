{-# LANGUAGE TemplateHaskell #-}

module Types where -- (ApplicationState(..), paused, Sort(..), Resource, Tick, SortValue(..)) where

import Lens.Micro.TH
import ListZipper
import Graphics.Vty (Key, Modifier)
import Brick (EventM)
import System.Random (RandomGen, StdGen)

type Resource = ()
type Tick = ()

data SortValue = SortValue {
    _value :: Int,
    _highlight :: Maybe String
    }
makeLenses ''SortValue

data Sort = Sort {
    _values   :: ListZipper [SortValue]
    }
makeLenses ''Sort

data ApplicationState =
  State {
      _sort    :: Sort,
      _paused  :: Bool,
      _done    :: Bool,
      _randGen :: StdGen
  }
makeLenses ''ApplicationState
