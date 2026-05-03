{-# LANGUAGE TypeFamilies, OverloadedStrings, OverloadedLists, DerivingVia #-}

module StarSet.Games.Stun
  ( stun
  ) where

import StarSet.Util.JSON
import StarSet.Game
import StarSet.Card.Set

import Data.List (nub, unzip4)
import GHC.Generics

import Miso (text)
import qualified Miso.Html as H
import qualified Miso.JSON as Miso
import qualified Data.Aeson as Aeson

data Stun = Stun deriving (Eq, Ord, Show)

instance Named Stun where name Stun = "STUN"

instance Game Stun where
  type Card Stun = SetCard
  type Collection Stun = (SetCard, SetCard, SetCard)
  type SpecificAchievement Stun = StunAchievement

  achievements Stun = [minBound..maxBound]
  completeAchievement Stun = Just Complete

  deck Stun = setDeck
  laidDown Stun = 9
  noSetAction Stun = AddMore 3
  minimumSet Stun = 3
  maximumSet Stun = Just 3
  makeSet Stun [a, b, c] = (a, b, c)
  makeSet Stun _ = error "Unexpected set size"
  isSet Stun (a, b, c) = lne2 ns && lne2 fs && lne2 ss && lne2 cs where
    (ns, fs, ss, cs) = unzip4 $ ((,,,) <$> cardNumber <*> cardFill <*> cardShape <*> cardColor) <$> [a, b, c]
    lne2 xs = length (nub xs) == 2
  setAchievements Stun _ = []

  styles Stun = setStyles
  renderCard Stun = renderSetCard

  rules Stun ex = H.div_ []
    [ H.div_ []
      [ text "The cards have four characteristics: number of shapes (1, 2, or 3); shape fill (empty, shaded, or filled); shape (oval, diamond, or tilde); color (red, green, or blue)."
      ]
    , H.div_ []
      [ text "A set is a group of three cards where, for each characteristic, two cards share a trait and the third has a different one."
      ]
    , H.div_ []
      [ text "For example, this is a set:"
      ]
    , ex
      [ renderCard Stun $ SetCard Two Empty Tilde Green
      , renderCard Stun $ SetCard Three Filled Oval Red
      , renderCard Stun $ SetCard Three Filled Tilde Green
      ]
    ]

data StunAchievement
  = Complete
  deriving (Eq, Ord, Generic, Enum, Bounded)
  deriving (Miso.FromJSON, Miso.ToJSON, Aeson.FromJSON, Aeson.ToJSON) via (ViaAeson StunAchievement)

instance Named StunAchievement where
  name Complete = "Stunned"

instance AchievementLike StunAchievement where
  description Complete = ["Complete a game of STUN."]

stun :: SomeGame
stun = SomeGame Stun
