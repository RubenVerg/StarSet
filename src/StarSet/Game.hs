{-# LANGUAGE TypeFamilies #-}

module StarSet.Game
  ( Game(..)
  , NoSetAction(..)
  , SomeGame(..)
  ) where

import Numeric.Natural
import Type.Reflection

import Data.Set (Set)
import Miso (View, CSS)
import Miso.JSON

data NoSetAction
  = Redeal
  | AddMore Natural

class (Typeable g, Eq g, Ord g, Eq (Card g), Ord (Card g), Show g, Show (Card g), FromJSON (Card g), ToJSON (Card g)) => Game g where
  type Card g
  type Collection g
  
  deck :: g -> Set (Card g)
  laidDown :: g -> Natural
  noSetAction :: g -> NoSetAction
  minimumSet :: g -> Natural
  maximumSet :: g -> Maybe Natural
  makeSet :: g -> Set (Card g) -> Collection g
  isSet :: g -> Collection g -> Bool

  styles :: g -> [CSS]
  renderCard :: g -> Card g -> View model action

  rules :: g -> View model action

data SomeGame where
  SomeGame :: Game g => g -> SomeGame

instance Show SomeGame where
  show (SomeGame g) = show g

instance Eq SomeGame where
  (SomeGame @a a) == (SomeGame @b b) = case eqTypeRep (typeRep @a) (typeRep @b) of
    Just HRefl -> a == b
    Nothing -> False