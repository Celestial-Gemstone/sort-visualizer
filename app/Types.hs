{-# LANGUAGE TemplateHaskell #-}

module Types where -- (ApplicationState(..), paused, Sort(..), Resource, Tick, SortValue(..)) where

import Lens.Micro.TH
import ListZipper
import System.Random (StdGen)
import Data.IORef (IORef)

type Resource = ()
type Tick = ()

data SortValue = SortValue
  { _value     :: Int
  , _highlight :: Maybe String
  }
makeLenses ''SortValue

data Sort = Sort
  { _values :: ListZipper [SortValue]
  , _paused :: Bool
  , _done   :: Bool
  }
makeLenses ''Sort

data ApplicationState = State
  { _sort      :: Sort
  , _tickDelay :: IORef Int
  , _randGen   :: StdGen
  }
makeLenses ''ApplicationState
