{-# LANGUAGE TypeFamilies, OverloadedStrings, OverloadedLists #-}

module StarSet.Games.NearSet
  ( nearSet
  ) where

import StarSet.Game
import StarSet.Card.Set

import Data.List (nub, unzip4, (\\))

import Miso (text)
import qualified Miso.Html as H

data NearSet = NearSet deriving (Eq, Ord, Show)

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

instance Game NearSet where
  type Card NearSet = SetCard
  type Collection NearSet = (SetCard, SetCard, SetCard)

  deck NearSet = setDeck
  laidDown NearSet = 12
  noSetAction NearSet = AddMore 3
  minimumSet NearSet = 3
  maximumSet NearSet = Just 3
  makeSet NearSet [a, b, c] = (a, b, c)
  makeSet NearSet _ = error "Unexpected set size"
  isSet NearSet = isNearSet

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

nearSet :: SomeGame
nearSet = SomeGame NearSet
