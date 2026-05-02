{-# LANGUAGE TypeFamilies, OverloadedStrings, OverloadedLists, DerivingVia #-}

module StarSet.Games.Set
  ( set
  ) where

import StarSet.Util.JSON
import StarSet.Game
import StarSet.Card.Set

import Data.List (nub, unzip4)
import GHC.Generics

import Miso (text)
import qualified Miso.Html as H
import qualified Data.Set as Set
import qualified Miso.JSON as Miso
import qualified Data.Aeson as Aeson

data Set = Set deriving (Eq, Ord, Show)

sameOrDifferent :: Eq a => [a] -> Bool
sameOrDifferent xs = let u = length $ nub xs in u == length xs || u == 1

instance Named Set where name Set = "SET"

instance Game Set where
  type Card Set = SetCard
  type Collection Set = (SetCard, SetCard, SetCard)
  type SpecificAchievement Set = SetAchievement

  achievements Set = [minBound..maxBound]
  completeAchievement Set = Just Complete

  deck Set = setDeck
  laidDown Set = 12
  noSetAction Set = AddMore 3
  minimumSet Set = 3
  maximumSet Set = Just 3
  makeSet Set [a, b, c] = (a, b, c)
  makeSet Set _ = error "Unexpected set size"
  isSet Set (a, b, c) = sameOrDifferent ns && sameOrDifferent fs && sameOrDifferent ss && sameOrDifferent cs where
    (ns, fs, ss, cs) = unzip4 $ ((,,,) <$> cardNumber <*> cardFill <*> cardShape <*> cardColor) <$> [a, b, c]
  setAchievements Set (a, b, c) = Set.fromList [ThreeOfAKind | threeOf] where
    (ns, fs, ss, cs) = unzip4 $ ((,,,) <$> cardNumber <*> cardFill <*> cardShape <*> cardColor) <$> [a, b, c]
    same xs = length (nub xs) == 1
    threeOf = same ns || same fs || same ss || same cs

  styles Set = setStyles
  renderCard Set = renderSetCard

  rules Set = H.div_ []
    [ H.div_ []
      [ text "The cards have four characteristics: number of shapes (1, 2, or 3); shape fill (empty, shaded, or filled); shape (oval, diamond, or tilde); color (red, green, or blue)."
      ]
    , H.div_ []
      [ text "A set is a group of three cards where, for each characteristic, the three cards either all share the same trait or all have different traits."
      ]
    ]

data SetAchievement
  = Complete
  | ThreeOfAKind
  deriving (Eq, Ord, Generic, Enum, Bounded)
  deriving (Miso.FromJSON, Miso.ToJSON, Aeson.FromJSON, Aeson.ToJSON) via (ViaAeson SetAchievement)

instance Named SetAchievement where
  name Complete = "I'm all set"
  name ThreeOfAKind = "Three of a kind"

instance AchievementLike SetAchievement where
  description Complete = ["Complete a game of SET."]
  description ThreeOfAKind = ["In a game of SET, find a set where at least one trait is the same among all three cards."]

set :: SomeGame
set = SomeGame Set
