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

  rules EvenQuads ex = H.div_ []
    [ H.div_ []
      [ text "The cards have three characteristics: number of shapes (1, 2, 3, or 4); shape (icosahedron, square, circle, or spiral); color (red, green, blue, or yellow)."
      ]
    , H.div_ []
      [ text "A set is a group of four cards where, for each characteristic, the three cards all share the same trait, or all have different traits, or have two of one trait and two of another."
      ]
    , H.div_ []
      [ text "For example, this is a set:"
      ]
    , ex
      [ renderCard EvenQuads $ QuadsCard Two Circle Red
      , renderCard EvenQuads $ QuadsCard One Square Green
      , renderCard EvenQuads $ QuadsCard One Icosahedron Yellow
      , renderCard EvenQuads $ QuadsCard Two Spiral Blue
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
