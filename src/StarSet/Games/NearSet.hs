{-# LANGUAGE TypeFamilies, OverloadedStrings, OverloadedLists, DerivingVia #-}

module StarSet.Games.NearSet
  ( nearSet
  , numberNearSet
  , fillNearSet
  , shapeNearSet
  , colorNearSet
  ) where

import StarSet.Util.JSON
import StarSet.Game
import StarSet.Card.Set

import Data.List (nub, unzip4, (\\))
import Data.Void
import Type.Reflection
import GHC.Generics

import Miso (text, toMisoString)
import qualified Miso.Html as H
import qualified Miso.JSON as Miso
import qualified Data.Aeson as Aeson

sameOrDifferent :: Eq a => [a] -> Bool
sameOrDifferent xs = let u = length $ nub xs in u == length xs || u == 1

isOriginalSet :: (SetCard, SetCard, SetCard) -> Bool
isOriginalSet (a, b, c) = sameOrDifferent ns && sameOrDifferent fs && sameOrDifferent ss && sameOrDifferent cs where
  (ns, fs, ss, cs) = unzip4 $ ((,,,) <$> cardNumber <*> cardFill <*> cardShape <*> cardColor) <$> [a, b, c]

variate :: (Enum a, Bounded a, Eq a) => SetProperty a -> SetCard -> [SetCard]
variate prop card = setProperty prop card <$> ([minBound..maxBound] \\ [getProperty prop card])

isNearSetOn :: (Enum a, Bounded a, Eq a) => SetProperty a -> (SetCard, SetCard, SetCard) -> Bool
isNearSetOn prop (a, b, c) = any isOriginalSet $ ((a, b, ) <$> variate prop c) ++ ((a, , c) <$> variate prop b) ++ ((, b, c) <$> variate prop a)

isNearSet :: (SetCard, SetCard, SetCard) -> Bool
isNearSet cards = isNearSetOn SetNumber cards || isNearSetOn SetFill cards || isNearSetOn SetShape cards || isNearSetOn SetColor cards

data NearSet = NearSet deriving (Eq, Ord, Show)

instance Named NearSet where name NearSet = "NearSet"

instance Game NearSet where
  type Card NearSet = SetCard
  type Collection NearSet = (SetCard, SetCard, SetCard)
  type SpecificAchievement NearSet = NearSetAchievement

  achievements NearSet = [minBound..maxBound]
  completeAchievement NearSet = Just Complete

  deck NearSet = setDeck
  laidDown NearSet = 12
  noSetAction NearSet = AddMore 3
  minimumSet NearSet = 3
  maximumSet NearSet = Just 3
  makeSet NearSet [a, b, c] = (a, b, c)
  makeSet NearSet _ = error "Unexpected set size"
  isSet NearSet = isNearSet
  setAchievements NearSet _ = []

  styles NearSet = setStyles
  renderCard NearSet = renderSetCard

  rules NearSet = H.div_ []
    [ H.div_ []
      [ text "The cards have four characteristics: number of shapes (1, 2, or 3); shape fill (empty, shaded, or filled); shape (oval, diamond, or tilde); color (red, green, or blue)"
      ]
    , H.div_ []
      [ text "A SET-set is a group of three cards where, for each characteristic, the three cards either all share the same trait or all have different traits."
      ]
    , H.div_ []
      [ text "A set is a group of three cards where you can variate exactly one of the four properties on exactly one of the three cards in such a way that they form a SET-set."
      ]
    ]

data NearSetAchievement
  = Complete
  deriving (Eq, Ord, Generic, Enum, Bounded)
  deriving (Miso.FromJSON, Miso.ToJSON, Aeson.FromJSON, Aeson.ToJSON) via (ViaAeson NearSetAchievement)

instance Named NearSetAchievement where
  name Complete = "Nearly..."

instance AchievementLike NearSetAchievement where
  description Complete = ["Complete a game of NearSet."]

newtype NearSetOn a = NearSetOn (SetProperty a)

instance Eq (NearSetOn a) where _ == _ = True
instance Ord (NearSetOn a) where compare _ _ = EQ
deriving instance Show (NearSetOn a)

instance Named (NearSetOn a) where
  name (NearSetOn prop) = "NearSet (" <> toMisoString (show prop) <> " only)"

instance (Enum a, Bounded a, Eq a, Typeable a) => Game (NearSetOn a) where
  type Card (NearSetOn a) = SetCard
  type Collection (NearSetOn a) = (SetCard, SetCard, SetCard)
  type SpecificAchievement (NearSetOn a) = Void

  achievements (NearSetOn _) = []
  completeAchievement (NearSetOn _) = Nothing

  deck (NearSetOn _) = setDeck
  laidDown (NearSetOn _) = 12
  noSetAction (NearSetOn _) = AddMore 3
  minimumSet (NearSetOn _) = 3
  maximumSet (NearSetOn _) = Just 3
  makeSet (NearSetOn _) [a, b, c] = (a, b, c)
  makeSet (NearSetOn _) _ = error "Unexpected set size"
  isSet (NearSetOn prop) = isNearSetOn prop
  setAchievements (NearSetOn _) _ = []

  styles (NearSetOn _) = setStyles
  renderCard (NearSetOn _) = renderSetCard

  rules (NearSetOn prop) = H.div_ []
    [ H.div_ []
      [ text "The cards have four characteristics: number of shapes (1, 2, or 3); shape fill (empty, shaded, or filled); shape (oval, diamond, or tilde); color (red, green, or blue)"
      ]
    , H.div_ []
      [ text "A SET-set is a group of three cards where, for each characteristic, the three cards either all share the same trait or all have different traits."
      ]
    , H.div_ []
      [ text $ toMisoString $ "A set is a group of three cards where you can variate the " ++ show prop ++ " on exactly one of the three cards in such a way that they form a SET-set."
      ]
    ]

nearSet :: SomeGame
nearSet = SomeGame NearSet

numberNearSet, fillNearSet, shapeNearSet, colorNearSet :: SomeGame
numberNearSet = SomeGame $ NearSetOn SetNumber
fillNearSet = SomeGame $ NearSetOn SetFill
shapeNearSet = SomeGame $ NearSetOn SetShape
colorNearSet = SomeGame $ NearSetOn SetColor
