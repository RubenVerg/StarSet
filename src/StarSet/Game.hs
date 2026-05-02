{-# LANGUAGE TypeFamilies, OverloadedStrings, ViewPatterns, DerivingVia, OverloadedLists #-}

module StarSet.Game
  ( Named(..)
  , Game(..)
  , NoSetAction(..)
  , SomeGame(..)
  , S(..)
  , Achievement(..)
  , AchievementLike(..)
  ) where

import {-# SOURCE #-} StarSet.Games
import StarSet.Util.JSON

import Numeric.Natural
import Type.Reflection
import Type.Reflection.Unsafe
import Data.Void
import Data.Maybe
import Data.List
import GHC.Generics (Generic)

import Data.Set (Set)
import Miso (View, CSS, MisoString, fromMisoString, toMisoString)
import qualified Miso.JSON as Miso
import qualified Data.Aeson as Aeson
import qualified Data.Text as T

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

instance AchievementLike Void where description = absurd

data S where S :: Game g => g -> SpecificAchievement g -> S

instance Eq S where
  (S @g1 g1 a1) == (S @g2 g2 a2) = case eqTypeRep (typeRep @g1) (typeRep @g2) of
    Just HRefl -> g1 == g2 && a1 == a2
    Nothing -> False

instance Ord S where
  (S @g1 g1 a1) `compare` (S @g2 g2 a2) = case eqTypeRep (typeRep @g1) (typeRep @g2) of
    Just HRefl -> g1 `compare` g2 <> a1 `compare` a2
    Nothing -> typeRepFingerprint (typeRep @g1) `compare` typeRepFingerprint (typeRep @g2)

instance Miso.FromJSON S where
  parseJSON (Miso.Array [Miso.String (flip lookup games . fromMisoString -> Just (SomeGame g)), specific]) = S g <$> Miso.parseJSON specific
  parseJSON _ = fail "Invalid achievement!"

instance Miso.ToJSON S where
  toJSON (S g specific) = Miso.Array [Miso.String $ toMisoString $ fst $ fromJust $ find ((== SomeGame g) . snd) games, Miso.toJSON specific]

instance Aeson.FromJSON S where
  parseJSON (Aeson.Array [Aeson.String (flip lookup games . T.unpack -> Just (SomeGame g)), specific]) = S g <$> Aeson.parseJSON specific
  parseJSON _ = fail "Invalid achievement!"

instance Aeson.ToJSON S where
  toJSON (S g specific) = Aeson.Array [Aeson.String $ T.pack $ fst $ fromJust $ find ((== SomeGame g) . snd) games, Aeson.toJSON specific]

data Achievement
  = CompleteGame
  | FindSet
  | Specific S
  deriving (Eq, Ord, Generic)
  deriving (Miso.FromJSON, Miso.ToJSON, Aeson.FromJSON, Aeson.ToJSON) via (ViaAeson Achievement)

instance Named Achievement where
  name CompleteGame = "Complete any game"
  name FindSet = "Find any set"
  name (Specific (S _ ach)) = name ach

instance AchievementLike Achievement where
  description CompleteGame = []
  description FindSet = []
  description (Specific (S _ ach)) = description ach

instance Show Achievement where show = fromMisoString . name
