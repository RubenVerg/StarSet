{-# LANGUAGE TypeFamilies, OverloadedStrings, OverloadedLists #-}

module StarSet.Games.Set
  ( set
  ) where

import StarSet.Game
import StarSet.Card.Set

import Data.List (nub, unzip4)

import Miso (text)
import qualified Miso.Html as H

data Set = Set deriving (Eq, Ord, Show)

sameOrDifferent :: Eq a => [a] -> Bool
sameOrDifferent xs = let u = length $ nub xs in u == length xs || u == 1

instance Game Set where
  type Card Set = SetCard
  type Collection Set = (SetCard, SetCard, SetCard)

  name Set = "SET"

  deck Set = setDeck
  laidDown Set = 12
  noSetAction Set = AddMore 3
  minimumSet Set = 3
  maximumSet Set = Just 3
  makeSet Set [a, b, c] = (a, b, c)
  makeSet Set _ = error "Unexpected set size"
  isSet Set (a, b, c) = sameOrDifferent ns && sameOrDifferent fs && sameOrDifferent ss && sameOrDifferent cs where
    (ns, fs, ss, cs) = unzip4 $ ((,,,) <$> cardNumber <*> cardFill <*> cardShape <*> cardColor) <$> [a, b, c]

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

set :: SomeGame
set = SomeGame Set
