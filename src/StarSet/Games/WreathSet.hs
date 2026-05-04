{-# LANGUAGE TypeFamilies, OverloadedStrings, OverloadedLists, DerivingVia #-}

module StarSet.Games.WreathSet
  ( wreathSet
  , tripleWreathSet
  ) where

import StarSet.Util.JSON
import StarSet.Card.Permutation
import StarSet.Game

import GHC.Generics

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Miso.JSON as Miso
import qualified Data.Aeson as Aeson
import Combinatorics

data WreathSet = WreathSet deriving (Eq, Ord, Show)
data TripleWreathSet = TripleWreathSet deriving (Eq, Ord, Show)

instance Named WreathSet where name WreathSet = "Wreath Set"

instance Game WreathSet where
  type Card WreathSet = PermutationCard 3 Bool
  type Collection WreathSet = Set (PermutationCard 3 Bool)
  type SpecificAchievement WreathSet = WreathSetAchievement

  achievements WreathSet = [minBound..maxBound]
  completeAchievement WreathSet = Just Complete

  deck WreathSet = permutationDeck
  laidDown WreathSet = 9
  noSetAction WreathSet = AddMore 3
  minimumSet WreathSet = 3
  maximumSet WreathSet = Nothing
  makeSet WreathSet = id
  isSet WreathSet cards = any ((== identityCard) . foldr1 composeCards) $ permuteFast $ Set.toList cards
  setAchievements WreathSet _ = []

  styles WreathSet = permutationStyles
  renderCard WreathSet = renderPermutationCard True

  rules WreathSet line ex =
    [ line "The cards have three lines representing a permutation of three elements, and each line has or doesn't have a final bead."
    , line "A set is a group of at least three cards that can be placed next to each other in such a way that the permutation composes to the identity permutation (all lines go back to where they started), and there is an even number of beads on each line."
    , line "For example, this is a set:"
    , ex
      [ PermutationCard (generate ([0, 2, 1] !!)) (generate ([False, False, True] !!))
      , PermutationCard (generate ([1, 2, 0] !!)) (generate ([True, False, False] !!))
      , PermutationCard (generate ([1, 0, 2] !!)) (generate ([False, True, True] !!))
      ]
    , line "The dot indicates the parity of the permutation and might be useful in spotting sets."
    ]

data WreathSetAchievement
  = Complete
  deriving (Eq, Ord, Generic, Enum, Bounded)
  deriving (Miso.FromJSON, Miso.ToJSON, Aeson.FromJSON, Aeson.ToJSON) via (ViaAeson WreathSetAchievement)

instance Named WreathSetAchievement where
  name Complete = "Like it's Christmas"

instance AchievementLike WreathSetAchievement where
  description Complete = ["Complete a game of Wreath Set."]

instance Named TripleWreathSet where name TripleWreathSet = "Triple Wreath Set"

instance Game TripleWreathSet where
  type Card TripleWreathSet = PermutationCard 3 Z3
  type Collection TripleWreathSet = Set (PermutationCard 3 Z3)
  type SpecificAchievement TripleWreathSet = TripleWreathSetAchievement

  achievements TripleWreathSet = [minBound..maxBound]
  completeAchievement TripleWreathSet = Just CompleteT

  deck TripleWreathSet = permutationDeck
  laidDown TripleWreathSet = 9
  noSetAction TripleWreathSet = AddMore 3
  minimumSet TripleWreathSet = 3
  maximumSet TripleWreathSet = Nothing
  makeSet TripleWreathSet = id
  isSet TripleWreathSet cards = any ((== identityCard) . foldr1 composeCards) $ permuteFast $ Set.toList cards
  setAchievements TripleWreathSet _ = []

  styles TripleWreathSet = permutationStyles
  renderCard TripleWreathSet = renderPermutationCard True

  rules TripleWreathSet line ex =
    [ line "The cards have three lines representing a permutation of three elements, and each line has no bead, or an empty bead, or a filled bead."
    , line "A filled bead and an empty bead cancel out; three beads of the same shading cancel out."
    , line "A set is a group of at least three cards that can be placed next to each other in such a way that the permutation composes to the identity permutation (all lines go back to where they started), and the beads on each line cancel out."
    , line "For example, this is a set:"
    , ex
      [ PermutationCard (generate ([1, 2, 0] !!)) (generate ([Empty, None, None] !!))
      , PermutationCard (generate ([1, 0, 2] !!)) (generate ([Filled, Empty, Empty] !!))
      , PermutationCard (generate ([0, 2, 1] !!)) (generate ([Empty, Empty, Filled] !!))
      ]
    , line "The dot indicates the parity of the permutation and might be useful in spotting sets."
    ]

data TripleWreathSetAchievement
  = CompleteT
  deriving (Eq, Ord, Generic, Enum, Bounded)
  deriving (Miso.FromJSON, Miso.ToJSON, Aeson.FromJSON, Aeson.ToJSON) via (ViaAeson TripleWreathSetAchievement)

instance Named TripleWreathSetAchievement where
  name CompleteT = "Triple Holiday"

instance AchievementLike TripleWreathSetAchievement where
  description CompleteT = ["Complete a game of Triple Wreath Set."]

wreathSet :: SomeGame
wreathSet = SomeGame WreathSet

tripleWreathSet :: SomeGame
tripleWreathSet = SomeGame TripleWreathSet
