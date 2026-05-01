{-# LANGUAGE TypeFamilies, OverloadedStrings, MagicHash, ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module StarSet.Game
  ( Named(..)
  , Game(..)
  , NoSetAction(..)
  , SomeGame(..)
  , Achievement(..)
  , AchievementLike(..)
  ) where

import {-# SOURCE #-} StarSet.Games

import Numeric.Natural
import Type.Reflection
import Type.Reflection.Unsafe
import Data.Void
import Data.Maybe
import Data.List
import GHC.Base (dataToTag#, Int(I#))

import Data.Set (Set)
import Miso (View, CSS, MisoString, fromMisoString, toMisoString)
import Miso.JSON

data NoSetAction
  = Redeal
  | AddMore Natural

class Named a where
  name :: a -> MisoString

class
  ( Typeable g
  , Ord g
  , Ord (Card g)
  , Show g
  , Show (Card g)
  , FromJSON (Card g)
  , ToJSON (Card g)
  , Named g
  , AchievementLike (SpecificAchievement g)
  ) => Game g where
  type Card g
  type Collection g
  type SpecificAchievement g

  achievements :: g -> Set (SpecificAchievement g)
  completeAchievement :: g -> Maybe (SpecificAchievement g)
  
  deck :: g -> Set (Card g)
  laidDown :: g -> Natural
  noSetAction :: g -> NoSetAction
  minimumSet :: g -> Natural
  maximumSet :: g -> Maybe Natural
  makeSet :: g -> Set (Card g) -> Collection g
  isSet :: g -> Collection g -> Bool
  setAchievements :: g -> Collection g -> Set (SpecificAchievement g)

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

class (Ord a, Named a, FromJSON a, ToJSON a) => AchievementLike a where
  description :: a -> [MisoString]

instance Named Void where name = absurd

instance FromJSON Void where parseJSON = const $ fail "parseJSON @Void"

instance ToJSON Void where toJSON = absurd

instance AchievementLike Void where description = absurd

data Achievement where
  CompleteGame :: Achievement
  FindSet :: Achievement
  Specific :: Game g => g -> SpecificAchievement g -> Achievement

instance Eq Achievement where
  (Specific @g1 g1 a1) == (Specific @g2 g2 a2) = case eqTypeRep (typeRep @g1) (typeRep @g2) of
    Just HRefl -> g1 == g2 && a1 == a2
    Nothing -> False
  x == y = I# (dataToTag# x) == I# (dataToTag# y)

instance Ord Achievement where
  (Specific @g1 g1 a1) `compare` (Specific @g2 g2 a2) = case eqTypeRep (typeRep @g1) (typeRep @g2) of
    Just HRefl -> g1 `compare` g2 <> a1 `compare` a2
    Nothing -> typeRepFingerprint (typeRep @g1) `compare` typeRepFingerprint (typeRep @g2)
  x `compare` y = I# (dataToTag# x) `compare` I# (dataToTag# y)

instance Named Achievement where
  name CompleteGame = "Complete any game"
  name FindSet = "Find any set"
  name (Specific _ ach) = name ach

instance FromJSON Achievement where
  parseJSON (String "complete-game") = pure CompleteGame
  parseJSON (String "find-set") = pure FindSet
  parseJSON (Array [String (flip lookup games . fromMisoString -> Just (SomeGame g)), specific]) = Specific g <$> parseJSON specific
  parseJSON _ = fail "Invalid achievement!"

instance ToJSON Achievement where
  toJSON CompleteGame = String "complete-game"
  toJSON FindSet = String "find-set"
  toJSON (Specific g specific) = Array [String $ toMisoString $ fst $ fromJust $ find ((== SomeGame g) . snd) games, toJSON specific]

instance AchievementLike Achievement where
  description CompleteGame = []
  description FindSet = []
  description (Specific _ ach) = description ach

instance Show Achievement where show = fromMisoString . name
