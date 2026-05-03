{-# LANGUAGE TypeFamilies, OverloadedStrings, OverloadedLists, DerivingVia #-}

module StarSet.Games.WreathSet
  ( wreathSet
  ) where

import StarSet.Util.JSON
import StarSet.Game

import Data.List (genericIndex)
import Numeric.Natural
import Control.Monad
import GHC.Generics

import Miso hiding (compose, (!!))
import qualified Miso.CSS as C
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import qualified Miso.Canvas as D
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Miso.JSON as Miso
import qualified Data.Aeson as Aeson
import Combinatorics

newtype WreathCard = WreathCard [(Natural, Bool)]
  deriving (Eq, Ord, Read, Show, Generic)
  deriving (Miso.FromJSON, Miso.ToJSON, Aeson.FromJSON, Aeson.ToJSON) via (ViaAeson WreathCard)

compose :: WreathCard -> WreathCard -> WreathCard
compose (WreathCard xs) (WreathCard ys) = WreathCard $ map (\(y, y1) -> case xs `genericIndex` y of (x, x1) -> (x, x1 /= y1)) ys

identity :: WreathCard
identity = WreathCard [(0, False), (1, False), (2, False)]

parity :: WreathCard -> Bool
parity (WreathCard xs) = odd $ inversions $ fst <$> xs where
  inversions [] = 0
  inversions (t:ts) = length (filter (< t) ts) + inversions ts

data WreathSet = WreathSet deriving (Eq, Ord, Show)

instance Named WreathSet where name WreathSet = "Wreath Set"

instance Game WreathSet where
  type Card WreathSet = WreathCard
  type Collection WreathSet = Set WreathCard
  type SpecificAchievement WreathSet = WreathSetAchievement

  achievements WreathSet = [minBound..maxBound]
  completeAchievement WreathSet = Just Complete

  deck WreathSet = Set.fromList $ (\cases
    [a, b, c] a1 b1 c1 -> WreathCard [(a, a1), (b, b1), (c, c1)]
    _ _ _ _ -> error "unreachable") <$> permuteFast [0, 1, 2] <*> [True, False] <*> [True, False] <*> [True, False]
  laidDown WreathSet = 9
  noSetAction WreathSet = AddMore 3
  minimumSet WreathSet = 3
  maximumSet WreathSet = Nothing
  makeSet WreathSet = id
  isSet WreathSet cards = any ((== identity) . foldr1 compose) $ permuteFast $ Set.toList cards
  setAchievements WreathSet _ = []

  styles WreathSet =
    [ Sheet $ C.sheet_
      [ C.selector_ ".card"
        [ C.width $ C.pct 100
        , C.height $ C.pct 100
        , "-webkit-user-select" =: "none"
        , "-moz-user-select" =: "none"
        , "-ms-user-select" =: "none"
        , "user-select" =: "none"
        ]
      ]
    ]
  renderCard WreathSet c@(WreathCard xs) = D.canvas [P.classes_ ["card"], P.width_ "500", P.height_ "700"]
    (const $ pure ())
    (\_ -> do
      D.clearRect (0, 0, 500, 700)
      D.lineWidth 10
      forM_ ([0, 1, 2] :: [Int]) $ \j -> do
        let (x, x1) = xs !! j
        let col = D.ColorArg $ [C.hex "e63946", C.hex "2d9a4e", C.hex "7b5ea7"] !! j
        D.strokeStyle col
        D.fillStyle col
        D.beginPath ()
        D.moveTo (0, 100 + fromIntegral j * 200)
        D.lineTo (150, 100 + fromIntegral j * 200)
        D.lineTo (350, 100 + fromIntegral x * 200)
        D.lineTo (500, 100 + fromIntegral x * 200)
        D.stroke ()
        when x1 $ do
          D.beginPath ()
          D.arc (450, 100 + fromIntegral x * 200, 25, 0, 2 * pi)
          D.fill ()
      D.strokeStyle $ D.ColorArg C.black
      D.fillStyle $ D.ColorArg C.red
      D.beginPath ()
      D.arc (250, 600, 50, 0, 2 * pi)
      D.stroke ()
      when (parity c) $ do
        D.beginPath ()
        D.arc (250, 600, 25, 0, 2 * pi)
        D.fill ())

  rules WreathSet ex = H.div_ []
    [ H.div_ []
      [ text "The cards have three lines representing a permutation of three elements, and each line has or doesn't have a final dot."
      ]
    , H.div_ []
      [ text "A set is a group of at least three cards that can be placed next to each other in such a way that the permutation composes to the identity permutation (all lines go back to where they started), and there is an even number of dots on each line."
      ]
    , H.div_ []
      [ text "For example, this is a set:"
      ]
    , ex
      [ renderCard WreathSet $ WreathCard [(0, False), (2, False), (1, True)]
      , renderCard WreathSet $ WreathCard [(1, True), (2, False), (0, False)]
      , renderCard WreathSet $ WreathCard [(1, False), (0, True), (2, True)]
      ]
    , H.div_ []
      [ text "The dot indicates the parity of the permutation and might be useful in spotting sets."
      ]
    ]

data WreathSetAchievement
  = Complete
  deriving (Eq, Ord, Generic, Enum, Bounded)
  deriving (Miso.FromJSON, Miso.ToJSON, Aeson.FromJSON, Aeson.ToJSON) via (ViaAeson WreathSetAchievement)

instance Named WreathSetAchievement where
  name Complete = "Like it's Christmas"

instance AchievementLike WreathSetAchievement where
  description Complete = ["Complete a game of Wreath Set."]

wreathSet :: SomeGame
wreathSet = SomeGame WreathSet
