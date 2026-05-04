{-# LANGUAGE TypeFamilies, OverloadedStrings, OverloadedLists, DerivingVia #-}

module StarSet.Games.SuperSet
  ( superSet
  ) where

import StarSet.Util.JSON
import StarSet.Game
import StarSet.Card.Set

import Data.List (find, (\\))
import Data.Maybe
import GHC.Generics

import qualified Miso.JSON as Miso
import qualified Data.Aeson as Aeson
import Combinatorics (variate)

data SuperSet = SuperSet deriving (Eq, Ord, Show)

thirdCard :: SetCard -> SetCard -> SetCard
thirdCard (SetCard n1 f1 s1 c1) (SetCard n2 f2 s2 c2) = let
  n = if n1 == n2 then n1 else fromJust $ find ((&&) <$> (/= n1) <*> (/= n2)) ([minBound..maxBound] :: [SetNumber])
  f = if f1 == f2 then f1 else fromJust $ find ((&&) <$> (/= f1) <*> (/= f2)) ([minBound..maxBound] :: [SetFill])
  s = if s1 == s2 then s1 else fromJust $ find ((&&) <$> (/= s1) <*> (/= s2)) ([minBound..maxBound] :: [SetShape])
  c = if c1 == c2 then c1 else fromJust $ find ((&&) <$> (/= c1) <*> (/= c2)) ([minBound..maxBound] :: [SetColor])
  in SetCard n f s c

instance Named SuperSet where name SuperSet = "SuperSet"

instance Game SuperSet where
  type Card SuperSet = SetCard
  type Collection SuperSet = (SetCard, SetCard, SetCard, SetCard)
  type SpecificAchievement SuperSet = SuperSetAchievement

  achievements SuperSet = [minBound..maxBound]
  completeAchievement SuperSet = Just Complete

  deck SuperSet = setDeck
  laidDown SuperSet = 9
  noSetAction SuperSet = AddMore 3
  minimumSet SuperSet = 4
  maximumSet SuperSet = Just 4
  makeSet SuperSet [a, b, c, d] = (a, b, c, d)
  makeSet SuperSet _ = error "Unexpected set size"
  isSet SuperSet (a, b, c, d) = any (\case
    [x, y] -> case [a, b, c, d] \\ [x, y] of
      [u, v] -> thirdCard x y == thirdCard u v
      _ -> False
    _ -> False) $ variate 2 [a, b, c, d]
  setAchievements SuperSet _ = []

  styles SuperSet = setStyles
  renderCard SuperSet = renderSetCard

  rules SuperSet line ex =
    [ line "The cards have four characteristics: number of shapes (1, 2, or 3); shape fill (empty, shaded, or filled); shape (oval, diamond, or tilde); color (red, green, or blue)."
    , line "A SET-set is a group of three cards where, for each characteristic, the three cards either all share the same trait or all have different traits."
    , line "Each group of two cards has the property that there exists exactly one other card in the deck that forms a SET-set with them."
    , line "A set is a group of four cards that can be split into two pairs whose third card to complete the SET-set is the same."
    , line "For example, this is a set:"
    , ex
      [ SetCard One Shaded Oval Blue
      , SetCard One Filled Tilde Green
      , SetCard Two Empty Tilde Green
      , SetCard Three Empty Oval Blue
      ]
    , line "because the following card creates a SET-set with both the first two and the last two."
    , ex
      [ SetCard One Empty Diamond Red
      ]
    ]

data SuperSetAchievement
  = Complete
  deriving (Eq, Ord, Generic, Enum, Bounded)
  deriving (Miso.FromJSON, Miso.ToJSON, Aeson.FromJSON, Aeson.ToJSON) via (ViaAeson SuperSetAchievement)

instance Named SuperSetAchievement where
  name Complete = "Super!"

instance AchievementLike SuperSetAchievement where
  description Complete = ["Complete a game of SuperSet."]

superSet :: SomeGame
superSet = SomeGame SuperSet
