{-# LANGUAGE TypeFamilies, OverloadedStrings, OverloadedLists, DerivingVia #-}

module StarSet.Games.ProSet
  ( proSet
  , sevenCardProSet
  ) where

import StarSet.Util.JSON
import StarSet.Game

import Data.Bits
import Numeric.Natural
import GHC.Generics

import Miso hiding (set)
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import qualified Miso.CSS as C
import qualified Data.Set as Set
import qualified Miso.JSON as Miso
import qualified Data.Aeson as Aeson

commonStyles :: [CSS]
commonStyles =
  [ Style "@font-face { font-family: ProSet; src: url('data:font/woff2;base64,d09GMk9UVE8AAAKwAAoAAAAABiQAAAJjAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAADYIcP0ZGVE0cBmAAgkIBNgIkAwgEBgWEEAcgG3EFAK4GbGM4eiGKURWlNEpjHYqqj65M/uLfeY8I4vf73+w595mmb1qd+BNiSUQSIWrGK40qmqiUbp5o1q7/tbZ3EjJD/JjvLnhlrjLdZE8Q8UYuRFpiOLy176fVb5jbskw79f35xblefLSFU+vzXJPPIkU+H/tR9apwFPU86dTX9XwEez4s0RkNlGZtrZ3Oj5B7mDogPoKIJoWiEBHNg5NKt+oLKElY+t/z/8J5guRJmkfY6/CtVq9Kfrv0hESfvA8eck0oYkhLwhifeMOlcC/yIx7txborJ53YzjEAm6kRczcSZAMCIC0HCmsdyY+mW0DfLv9t4XI3ZLf/BL6lR9J/b4py/vZW+riL5cwCG9GIG7Ch3WZArWoYBjQYoSFToV6J7yU5c6kq4dqLa83rOgj43nn9IfzojDxYrxo9CT3/ivuoQrpJfsRtudP4hAchqTQABADsuWT0cXNWJ8+UVPOMDwjIM4AZCjIyJihggyYwQTcDCoXA2bI8/ubyoQCiSsh+WJB3DgSvDvVbb+NPvZV/jWQBpXNnngF1CQRjU/0FWFIF8FKplq5nDyR0PReAURk+QsM5JDVXkC36otCIJip6sWl3VSOOQBNFA2sgCEMVJF2Xke32QmHoh4qpiN9b1TAelE3N7C04LLYVEtYTQQRGyIkjC2td0gSm9XOBgyZWHCsjUp+AKb5rH8myNtKxgO+idQLbTU2s7E9asEhESGJICbVslA0KXEJGgq2H1zsVvWBhSRJJRiVioPo1wROZBOVIXIlDtYUlh1sHwjCck5aHeGarMMaTbIvUYjqDvLI1EAAAAA=='); }"
  , Sheet $ C.sheet_
    [ C.selector_ ".empty"
      [ "-webkit-text-stroke" =: "3px var(--dot-color)"
      , "-webkit-text-fill-color" =: "transparent"
      ]
    , C.selector_ ".filled"
      [ "-webkit-text-stroke" =: "3px var(--dot-color)"
      , "-webkit-text-fill-color" =: "var(--dot-color)"
      ]
    , C.selector_ ".card :nth-child(1)"
      [ "--dot-color" =: "#7b5ea7"
      ]
    , C.selector_ ".card :nth-child(2)"
      [ "--dot-color" =: "#0e76e8"
      ]
    , C.selector_ ".card :nth-child(3)"
      [ "--dot-color" =: "#2d9a4e"
      ]
    , C.selector_ ".card :nth-child(4)"
      [ "--dot-color" =: "#eae23e"
      ]
    , C.selector_ ".card :nth-child(5)"
      [ "--dot-color" =: "#ffa102"
      ]
    , C.selector_ ".card :nth-child(6)"
      [ "--dot-color" =: "#e63946"
      ]
    , C.selector_ ".card :nth-child(7)"
      [ "--dot-color" =: "#c03db8"
      ]
    , C.selector_ ".card :nth-child(8)"
      [ "--dot-color" =: "#9cec3b"
      ]
    , C.selector_ ".card :nth-child(9)"
      [ "--dot-color" =: "#3a3a3a"
      ]
    , C.selector_ ".card :nth-child(10)"
      [ "--dot-color" =: "#b9b9b9"
      ]
    , C.selector_ ".card"
      [ "container-type" =: "size"
      , C.fontFamily "ProSet"
      , C.width $ C.pct 100 
      , C.height $ C.pct 100
      , C.display "grid"
      , C.justifyItems "center"
      , C.alignItems "center"
      , C.gridTemplateColumns "repeat(2, 1fr)"
      , C.verticalAlign "top"
      , "-webkit-user-select" =: "none"
      , "-moz-user-select" =: "none"
      , "-ms-user-select" =: "none"
      , "user-select" =: "none"
      ]
    , C.selector_ ".card *"
      [ C.fontSize "35cqw"
      ]
    ]
  ]

commonRender :: Natural -> Natural -> View model action
commonRender n x = let
  bits = testBit x <$> [0..fromIntegral n - 1] :: [Bool]
  spans = (\o -> H.span_ [P.classes_ [if o then "filled" else "empty"]] [text "a"]) <$> bits
  in H.div_ [P.classes_ ["card"]] spans

newtype ProSet = ProSet Natural deriving (Eq, Ord, Show)
data SevenCardProSet = SevenCardProSet deriving (Eq, Ord, Show)

instance Named ProSet where
  name (ProSet 6) = "ProSet"
  name (ProSet n) = "ProSet (" <> toMisoString (show n) <> " dots)"

instance Game ProSet where
  type Card ProSet = Natural
  type Collection ProSet = (Natural, Natural, Natural)
  type SpecificAchievement ProSet = ProSetAchievement

  achievements (ProSet n) = [Complete n]
  completeAchievement (ProSet n) = Just $ Complete n

  deck (ProSet n) = [1..2 ^ n - 1]
  laidDown (ProSet _) = 12
  noSetAction (ProSet _) = AddMore 3
  minimumSet (ProSet _) = 3
  maximumSet (ProSet _) = Just 3
  makeSet (ProSet _) [a, b, c] = (a, b, c)
  makeSet (ProSet _) _ = error "Unexpected set size"
  isSet (ProSet _) (a, b, c) = a `xor` b `xor` c == 0
  setAchievements (ProSet _) _ = []

  styles (ProSet _) = commonStyles
  renderCard (ProSet n) = commonRender n

  rules (ProSet n) ex = H.div_ []
    [ H.div_ []
      [ text $ toMisoString $ "The cards have " ++ show n ++ " dots, each of which can be empty or filled."
      ]
    , H.div_ []
      [ text "A set is a group of three cards where, for each dot, exactly two or exactly zero cards have that dot filled."
      ]
    , H.div_ []
      [ text "For example, this is a set:"
      ]
    , ex
      [ renderCard (ProSet n) 14
      , renderCard (ProSet n) 42
      , renderCard (ProSet n) 36
      ]
    ]

newtype ProSetAchievement
  = Complete Natural
  deriving (Eq, Ord, Generic)
  deriving (Miso.FromJSON, Miso.ToJSON, Aeson.FromJSON, Aeson.ToJSON) via (ViaAeson ProSetAchievement)

instance Named ProSetAchievement where
  name (Complete 6) = "Professional"
  name (Complete n) = toMisoString (show n) <> "-dotted Professional"

instance AchievementLike ProSetAchievement where
  description (Complete 6) = ["Complete a game of ProSet."]
  description (Complete n) = ["Complete a game of ProSet with " <> toMisoString (show n) <> " dots."]

instance Named SevenCardProSet where name SevenCardProSet = "7-card ProSet"

instance Game SevenCardProSet where
  type Card SevenCardProSet = Natural
  type Collection SevenCardProSet = [Natural]
  type SpecificAchievement SevenCardProSet = SevenCardProSetAchievement

  achievements SevenCardProSet = [minBound..maxBound]
  completeAchievement SevenCardProSet = Just Complete7

  deck SevenCardProSet = [1..2 ^ (6 :: Int) - 1]
  laidDown SevenCardProSet = 7
  noSetAction SevenCardProSet = Redeal
  minimumSet SevenCardProSet = 3
  maximumSet SevenCardProSet = Nothing
  makeSet SevenCardProSet = Set.toList
  isSet SevenCardProSet = (== 0) . foldr1 xor
  setAchievements SevenCardProSet _ = []

  styles SevenCardProSet = commonStyles
  renderCard SevenCardProSet = commonRender 6

  rules SevenCardProSet ex = H.div_ []
    [ H.div_ []
      [ text "The cards have 6 dots, each of which can be empty or filled."
      ]
    , H.div_ []
      [ text "A set is a group of at least three cards where, for each dot, an even number of cards has that dot filled."
      ]
    , H.div_ []
      [ text "For example, this is a set:"
      ]
    , ex
      [ renderCard SevenCardProSet 62
      , renderCard SevenCardProSet 42
      , renderCard SevenCardProSet 22
      , renderCard SevenCardProSet 2
      ]
    ]

data SevenCardProSetAchievement
  = Complete7
  deriving (Eq, Ord, Generic, Enum, Bounded)
  deriving (Miso.FromJSON, Miso.ToJSON, Aeson.FromJSON, Aeson.ToJSON) via (ViaAeson SevenCardProSetAchievement)

instance Named SevenCardProSetAchievement where
  name Complete7 = "Cloud Seven"

instance AchievementLike SevenCardProSetAchievement where
  description Complete7 = ["Complete a game of 7-card ProSet."]

proSet :: Natural -> SomeGame
proSet = SomeGame . ProSet

sevenCardProSet :: SomeGame
sevenCardProSet = SomeGame SevenCardProSet
