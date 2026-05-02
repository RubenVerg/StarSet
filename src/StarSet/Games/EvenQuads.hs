{-# LANGUAGE TypeFamilies, OverloadedStrings, OverloadedLists, DerivingVia #-}

module StarSet.Games.EvenQuads
  ( evenQuads
  ) where

import StarSet.Util.JSON
import StarSet.Game

import Data.List (sort)
import GHC.Generics

import Miso
import qualified Miso.CSS as C
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import qualified Data.Set as Set
import qualified Miso.JSON as Miso
import qualified Data.Aeson as Aeson

data QuadsNumber = One | Two | Three | Four
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic)
  deriving (Miso.FromJSON, Miso.ToJSON, Aeson.FromJSON, Aeson.ToJSON) via (ViaAeson QuadsNumber)

data QuadsShape = Icosahedron | Square | Circle | Spiral
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic)
  deriving (Miso.FromJSON, Miso.ToJSON, Aeson.FromJSON, Aeson.ToJSON) via (ViaAeson QuadsShape)

data QuadsColor = Red | Green | Blue | Yellow
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic)
  deriving (Miso.FromJSON, Miso.ToJSON, Aeson.FromJSON, Aeson.ToJSON) via (ViaAeson QuadsColor)

data QuadsCard = QuadsCard
  { cardNumber :: QuadsNumber
  , cardShape :: QuadsShape
  , cardColor :: QuadsColor
  }
  deriving (Eq, Ord, Read, Show, Generic)
  deriving (Miso.FromJSON, Miso.ToJSON, Aeson.FromJSON, Aeson.ToJSON) via (ViaAeson QuadsCard)

data EvenQuads = EvenQuads deriving (Eq, Ord, Show)

instance Named EvenQuads where name EvenQuads = "EvenQuads"

instance Game EvenQuads where
  type Card EvenQuads = QuadsCard
  type Collection EvenQuads = (QuadsCard, QuadsCard, QuadsCard, QuadsCard)
  type SpecificAchievement EvenQuads = EvenQuadsAchievement

  achievements EvenQuads = [minBound..maxBound]
  completeAchievement EvenQuads = Just Complete

  deck EvenQuads = Set.fromList $ QuadsCard <$> [minBound..maxBound] <*> [minBound..maxBound] <*> [minBound..maxBound]
  laidDown EvenQuads = 9
  noSetAction EvenQuads = AddMore 3
  minimumSet EvenQuads = 4
  maximumSet EvenQuads = Just 4
  makeSet EvenQuads [a, b, c, d] = (a, b, c, d)
  makeSet EvenQuads _ = error "Unexpected set size"
  isSet EvenQuads (a, b, c, d) = s ns && s ss && s cs where
    (ns, ss, cs) = unzip3 $ ((,,) <$> cardNumber <*> cardShape <*> cardColor) <$> [a, b, c, d]
    s xs = sort (fmap (\q -> length $ filter (== q) xs) [minBound..maxBound]) `elem` ([[0, 0, 0, 4], [0, 0, 2, 2], [1, 1, 1, 1]] :: [[Int]])
  setAchievements EvenQuads _ = []

  styles EvenQuads =
    [ Style "@font-face { font-family: EvenQuads; src: url('data:font/woff2;base64,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'); }"
    , Sheet $ C.sheet_
      [ C.selector_ ".red"
        [ "color" =: "#e63946"
        ]
      , C.selector_ ".green"
        [ "color" =: "#2d9a4e"
        ]
      , C.selector_ ".blue"
        [ "color" =: "#7b5ea7"
        ]
      , C.selector_ ".yellow"
        [ "color" =: "#e9c46a"
        ]
      , C.selector_ ".card"
        [ "container-type" =: "size"
        , C.fontFamily "EvenQuads"
        , C.width $ C.pct 100 
        , C.height $ C.pct 100
        , C.display "inline-flex"
        , C.justifyContent "center"
        , C.alignItems "center"
        , C.verticalAlign "top"
        , "-webkit-user-select" =: "none"
        , "-moz-user-select" =: "none"
        , "-ms-user-select" =: "none"
        , "user-select" =: "none"
        ]
      , C.selector_ ".card *"
        [ C.fontSize "35cqw"
        ]
      , C.selector_ ".card > span" 
        [ C.display "grid"
        , C.gap "5cqw"
        ]
      , C.selector_ ".card > span > *"
        [ C.margin "auto"
        ]
      , C.selector_ ".card > span > :nth-child(1)"
        [ "grid-area" =: "a"
        ]
      , C.selector_ ".card > span > :nth-child(2)"
        [ "grid-area" =: "b"
        ]
      , C.selector_ ".card > span > :nth-child(3)"
        [ "grid-area" =: "c"
        ]
      , C.selector_ ".card > span > :nth-child(4)"
        [ "grid-area" =: "d"
        ]
      , C.selector_ ".one"
        [ "grid-template-areas" =: "'a'"
        ]
      , C.selector_ ".two"
        [ "grid-template-areas" =: "'a' 'b'"
        ]
      , C.selector_ ".three"
        [ "grid-template-areas" =: "'a b' 'c c'"
        ]
      , C.selector_ ".four"
        [ "grid-template-areas" =: "'a b' 'c d'"
        ]
      ]
    ]
  renderCard EvenQuads (QuadsCard n s c) = let
    symbol = case s of
      Icosahedron -> 'a'
      Square -> 'b'
      Circle -> 'c'
      Spiral -> 'd'
    (num, nc) = case n of
      One -> (1, "one")
      Two -> (2, "two")
      Three -> (3, "three")
      Four -> (4, "four")
    col = case c of
      Red -> "red"
      Green -> "green"
      Blue -> "blue"
      Yellow -> "yellow"
    sp = replicate num $ H.span_ [] [text $ toMisoString symbol]
    in H.div_ [P.classes_ ["card", col]] [H.span_ [P.classes_ [nc]] sp]

  rules EvenQuads = H.div_ []
    [ H.div_ []
      [ text "The cards have three characteristics: number of shapes (1, 2, 3, or 4); shape (icosahedron, square, circle, or spiral); color (red, green, blue, or yellow)."
      ]
    , H.div_ []
      [ text "A set is a group of four cards where, for each characteristic, the three cards all share the same trait, or all have different traits, or have two of one trait and two of another."
      ]
    ]

data EvenQuadsAchievement
  = Complete
  deriving (Eq, Ord, Generic, Enum, Bounded)
  deriving (Miso.FromJSON, Miso.ToJSON, Aeson.FromJSON, Aeson.ToJSON) via (ViaAeson EvenQuadsAchievement)

instance Named EvenQuadsAchievement where
  name Complete = "Quad Power"

instance AchievementLike EvenQuadsAchievement where
  description Complete = ["Complete a game of EvenQuads."]

evenQuads :: SomeGame
evenQuads = SomeGame EvenQuads
