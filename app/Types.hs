{-# LANGUAGE TemplateHaskell #-}

module Types where -- (ApplicationState(..), paused, Sort(..), Resource, Tick, SortValue(..)) where

import Lens.Micro.TH
import ListZipper
import System.Random (StdGen)
import Control.Concurrent (MVar)
import Data.IORef (IORef)

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
      _sort      :: Sort,
      _paused    :: Bool,
      _done      :: Bool,
      _tickDelay :: IORef Int,
      _randGen   :: StdGen
  }
makeLenses ''ApplicationState
