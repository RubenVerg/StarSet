{-# LANGUAGE TypeFamilies, OverloadedStrings, OverloadedLists, DerivingVia, QuasiQuotes #-}

module StarSet.Games.C53T
  ( c53t
  ) where

import StarSet.Util.JSON
import StarSet.Game

import GHC.Generics

import Data.String.Interpolate (i)
import Miso hiding (compose, (!!), ms)
import qualified Miso.CSS as C
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import qualified Miso.Svg as S
import qualified Miso.Svg.Property as P
import qualified Data.Set as Set
import qualified Miso.JSON as Miso
import qualified Data.Aeson as Aeson

data C53TOrientation
  = Zero
  | One
  | Two
  | Three
  | Four
  deriving (Eq, Ord, Read, Show, Generic, Enum, Bounded)
  deriving (Miso.FromJSON, Miso.ToJSON, Aeson.FromJSON, Aeson.ToJSON) via (ViaAeson C53TOrientation)

data C53TCard = C53TCard
  { cardBottom :: C53TOrientation
  , cardMiddle :: C53TOrientation
  , cardTop :: C53TOrientation
  }
  deriving (Eq, Ord, Read, Show, Generic)
  deriving (Miso.FromJSON, Miso.ToJSON, Aeson.FromJSON, Aeson.ToJSON) via (ViaAeson C53TCard)

data C53T = C53T deriving (Eq, Ord, Show)

instance Named C53T where name C53T = "C53T"

instance Game C53T where
  type Card C53T = C53TCard
  type Collection C53T = (C53TCard, C53TCard, C53TCard, C53TCard, C53TCard)
  type SpecificAchievement C53T = C53TAchievement

  achievements C53T = [minBound..maxBound]
  completeAchievement C53T = Just Complete

  deck C53T = Set.fromList $ C53TCard <$> [minBound..maxBound] <*> [minBound..maxBound] <*> [minBound..maxBound]
  laidDown C53T = 12
  noSetAction C53T = AddMore 3
  minimumSet C53T = 5
  maximumSet C53T = Just 5
  makeSet C53T [a, b, c, d, e] = (a, b, c, d, e)
  makeSet C53T _ = error "Unexpected set size"
  isSet C53T (a, b, c, d, e) = sumToZero bs && sumToZero ms && sumToZero ts where
    (bs, ms, ts) = unzip3 $ ((,,) <$> cardBottom <*> cardMiddle <*> cardTop) <$> [a, b, c, d, e]
    sumToZero xs = sum (fromEnum <$> xs) `mod` 5 == 0
  setAchievements C53T _ = []

  styles C53T =
    [ Sheet $ C.sheet_
      [ C.selector_ ".spoke0"
        [ "--spoke-color" =: "#e63946"
        ]
      , C.selector_ ".spoke1"
        [ "--spoke-color" =: "#7b5ea7"
        ]
      , C.selector_ ".spoke2"
        [ "--spoke-color" =: "#2d4f85"
        ]
      , C.selector_ ".spoke3"
        [ "--spoke-color" =: "#2d9a4e"
        ]
      , C.selector_ ".spoke4"
        [ "--spoke-color" =: "#e9c46a"
        ]
      , C.selector_ ".bottom"
        [ C.stroke "#b0b2b4"
        ]
      , C.selector_ ".middle"
        [ C.stroke "#73777e"
        ]
      , C.selector_ ".top"
        [ C.stroke "#3b4048"
        ]
      , C.selector_ ".card"
        [ "container-type" =: "size"
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
      , C.selector_ ".card svg"
        [ C.width "45cqw"
        ]
      ]
    ]
  renderCard C53T (C53TCard a b c) = H.div_ [P.classes_ ["card"]] [H.span_ []
    [ renderPentagon "bottom" a
    , H.br_ []
    , renderPentagon "middle" b
    , H.br_ []
    , renderPentagon "top" c
    ]] where
    st = 2 * pi / 5 :: Double
    (x0, y0) = (0 :: Double, -1 :: Double)
    (x1, y1) = (sin st, negate $ cos st)
    (x2, y2) = (sin $ 2 * st, negate $ cos $ 2 * st)
    (x3, y3) = (negate x2, y2)
    (x4, y4) = (negate x1, y1)
    xs = [x0, x1, x2, x3, x4]
    ys = [y0, y1, y2, y3, y4]
    renderPentagon layer o = S.svg_ [P.viewBox_ "-1.25 -1.25 2.5 2.5"]
      [ S.path_
        [ P.d_ $ toMisoString ([i|
          M #{x0} #{y0}
            #{x1} #{y1}
            #{x2} #{y2}
            #{x3} #{y3}
            #{x4} #{y4}
          Z
          |] :: String)
        , P.classes_ [layer]
        , P.fill_ "transparent"
        , P.strokeWidth_ "0.075cqw"
        ]
      , S.path_
        [ P.d_ $ toMisoString ([i|M 0 0 #{xs !! fromEnum o} #{ys !! fromEnum o}|] :: String)
        , P.classes_ ["spoke" <> toMisoString (fromEnum o)]
        , P.fill_ "transparent"
        , P.stroke_ "var(--spoke-color)"
        , P.strokeWidth_ "0.1cqw"
        ]
      , S.circle_
        [ P.cx_ $ toMisoString $ xs !! fromEnum o
        , P.cy_ $ toMisoString $ ys !! fromEnum o
        , P.r_ "0.1cqw"
        , P.classes_ ["spoke" <> toMisoString (fromEnum o)]
        , P.fill_ "var(--spoke-color)"
        ]
      ]

  rules C53T ex = H.div_ []
    [ H.div_ []
      [ text "The cards have three pentagons, each with a colored spoke going from the center to one of the vertices."
      ]
    , H.div_ []
      [ text "A set is a group of at five cards where, for each pentagon, the colored spokes are laid out in a way that they have an axis of symmetry"
      ]
    , H.div_ []
      [ text "For example, this is a set:"
      ]
    , ex
      [ renderCard C53T $ C53TCard Zero Two Zero
      , renderCard C53T $ C53TCard One Four Two
      , renderCard C53T $ C53TCard Zero One Three
      , renderCard C53T $ C53TCard Zero Two One
      , renderCard C53T $ C53TCard Four One Four
      ]
    ]

data C53TAchievement
  = Complete
  deriving (Eq, Ord, Generic, Enum, Bounded)
  deriving (Miso.FromJSON, Miso.ToJSON, Aeson.FromJSON, Aeson.ToJSON) via (ViaAeson C53TAchievement)

instance Named C53TAchievement where
  name Complete = "See-Set"

instance AchievementLike C53TAchievement where
  description Complete = ["Complete a game of C53T."]

c53t :: SomeGame
c53t = SomeGame C53T
