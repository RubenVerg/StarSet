{-# LANGUAGE DeriveAnyClass, OverloadedStrings #-}

module StarSet.Card.Set
  ( SetNumber(..)
  , SetFill(..)
  , SetShape(..)
  , SetColor(..)
  , SetCard(..)
  , setDeck
  , setStyles
  , renderSetCard
  , SetProperty(..)
  , getProperty
  , setProperty
  ) where

import Data.List
import GHC.Generics

import Miso hiding (getProperty)
import Miso.JSON
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import qualified Miso.CSS as C
import qualified Data.Set as Set

data SetNumber = One | Two | Three deriving (Eq, Ord, Read, Show, Enum, Bounded)
instance FromJSON SetNumber where
  parseJSON (Number 1) = pure One
  parseJSON (Number 2) = pure Two
  parseJSON (Number 3) = pure Three
  parseJSON _ = fail "Invalid Number"
instance ToJSON SetNumber where
  toJSON One = Number 1
  toJSON Two = Number 2
  toJSON Three = Number 3

data SetFill = Empty | Shaded | Filled deriving (Eq, Ord, Read, Show, Enum, Bounded)
instance FromJSON SetFill where
  parseJSON (String "empty") = pure Empty
  parseJSON (String "shaded") = pure Shaded
  parseJSON (String "filled") = pure Filled
  parseJSON _ = fail "Invalid Fill"
instance ToJSON SetFill where
  toJSON Empty = String "empty"
  toJSON Shaded = String "shaded"
  toJSON Filled = String "filled"

data SetShape = Diamond | Oval | Tilde deriving (Eq, Ord, Read, Show, Enum, Bounded)
instance FromJSON SetShape where
  parseJSON (String "diamond") = pure Diamond
  parseJSON (String "oval") = pure Oval
  parseJSON (String "tilde") = pure Tilde
  parseJSON _ = fail "Invalid Shape"
instance ToJSON SetShape where
  toJSON Diamond = String "diamond"
  toJSON Oval = String "oval"
  toJSON Tilde = String "tilde"
  
data SetColor = Red | Green | Blue deriving (Eq, Ord, Read, Show, Enum, Bounded)
instance FromJSON SetColor where
  parseJSON (String "red") = pure Red
  parseJSON (String "green") = pure Green
  parseJSON (String "blue") = pure Blue
  parseJSON _ = fail "Invalid Color"
instance ToJSON SetColor where
  toJSON Red = String "red"
  toJSON Green = String "green"
  toJSON Blue = String "blue"

data SetCard = SetCard
  { cardNumber :: SetNumber
  , cardFill :: SetFill
  , cardShape :: SetShape
  , cardColor :: SetColor
  } deriving (Eq, Ord, Read, Show, Generic, FromJSON, ToJSON)

setDeck :: Set.Set SetCard
setDeck = Set.fromList $ SetCard <$> [minBound..maxBound] <*> [minBound..maxBound] <*> [minBound..maxBound] <*> [minBound..maxBound]

setStyles :: [CSS]
setStyles =
  [ Style "@font-face { font-family: SET; src: url('data:font/woff2;base64,d09GMk9UVE8AAAPsAAoAAAAAB9gAAAOfAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAADYVLP0ZGVE0cBmAAgkIBNgIkAwwEBgWEEAcgGyQHIC4KY2PW2EMMwiRiUWnOS6/c7R2fNB7+f+1/+8zMdx9EJYkm0eqJSrOXEW0sKpUm9iVERJtJ/YjfA728f2+lUjn5QtNrrZ2HC2uDOGYstHXiBLCBAOtStkqVarUC+LdibRXVkPxmaNDjO7CopOcboonOEIndvP10hkapJCIpUBOlJBbhtJgYsJP+TaEoRET9LoXN3VIxIGXCvJ9qfhrFh3H2YZJ/6BeNjXHqT1zJOPi1XFUqaSkWaLVUt71lUZIn2lEyoZ8lJ8dNu5T9Ix7D9CfjiiBx/CkArMHFQ82Eqbx0ZT2nB3/yB2NwsKxhDuaD3SVD9jFR/uysHvyb0/XBFyz7y69p/kEUvwv/1nbvz9r/clXzn1hyKk6nmv2H5ayrv5a7/oz/yUzNvkH6f9MA0z/WIz/Gx6X8XLVVnUJLw+qU7cARqmDqdHp2XPN/1XRFSjiETkr+VP1oUBtoxY4Z20jCn1m4o2Cho+z+p83Sb7Pr4hoDD1ZRL7YUWs/StWGdk1jQUvamhFVhbCYLg55LD2FqbNXcyouTSgvlSpK9TvxQ7R/eid3WZrQQvwuOFz+tNknWsehqJt2ENewp+tRg1D0LFY+x2OWdu/nfUMWGxK3iaOI+k8hKPHb5f2MlyWn8kFDD0RpYiQv8V0XBIeAI4Vc98RW6WGiETSi01l1Nwiv+6KX0OCZNnVKbUKaFmCEOAyCqYmCMKCPQM25IRoiUpptMJMJjytrmsuag83TXn9jTvwtPH+Tq0431OLsda2wsa/YbpiobmxDwvunWZfjQOP0LqmrGSyhqHsQFVWSnsvs4U6bilQVBpoWr6uAL/FHeepifCqX2ScnHp+Wi8/E2EJCPAxiPhBw5SiSwGkWgRDUeKBQCB1IA+9L/fgFENWH2syLLlXT0l1/7Kh1NaFn9jdmQfzoj69Ly+dV8PFArA8FZw4gssEh0AMi2bDNQuxUUaHZdAPq4IynU2U+ZGscoN8UBFeqinqq0xOJkdd29FyiiqMNCUCm086dMsyOU26KSCu3eqMrY0P7eqrUHD7iUVPKkExMSyYSwk8IeggXF4skJIl2WZDgFuB3+LMAuCpnEMsngUmHgNvnWdjihLDJJ0sEv2XUBNlCacmg3nRAePtIFCuE7wzGIhBSHztw5Jwv4QC4DDqQLcYTlgiK+SAIMyfams7tzxnNttLu76TKIU5tEoFBYSnCUgWfZMopiLvOFUIfF3OFF0g0LgVAAAAA='); }"
  , Sheet $ C.sheet_
    [ C.selector_ ".empty"
      [ "-webkit-text-stroke" =: "3px var(--card-color)"
      , "-webkit-text-fill-color" =: "transparent"
      ]
    , C.selector_ ".shaded"
      [ "-webkit-text-stroke" =: "3px var(--card-color)"
      , "-webkit-text-fill-color" =: "transparent"
      , C.backgroundClip "text"
      , C.backgroundImage "repeating-linear-gradient(90deg, transparent, transparent 3px, var(--card-color) 3px, var(--card-color) 5px)"
      ]
    , C.selector_ ".filled"
      [ "-webkit-text-stroke" =: "3px var(--card-color)"
      , "-webkit-text-fill-color" =: "var(--card-color)"
      ]
    , C.selector_ ".red"
      [ "--card-color" =: "#e63946"
      ]
    , C.selector_ ".green"
      [ "--card-color" =: "#2d9a4e"
      ]
    , C.selector_ ".blue"
      [ "--card-color" =: "#7b5ea7"
      ]
    , C.selector_ ".card"
      [ C.fontSize $ C.em 4
      , C.fontFamily "SET"
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
    ]
  ]

renderSetCard :: SetCard -> View model action
renderSetCard (SetCard n f s c) = let
  symbol = case s of
    Diamond -> 'a'
    Oval -> 'b'
    Tilde -> 'c'
  num = case n of
    One -> 1
    Two -> 2
    Three -> 3
  fill = case f of
    Empty -> "empty"
    Shaded -> "shaded"
    Filled -> "filled"
  col = case c of
    Red -> "red"
    Green -> "green"
    Blue -> "blue"
  sp = intersperse (H.br_ []) $ replicate num (H.span_ [P.classes_ [fill]] [text $ toMisoString symbol])
  in H.div_ [P.classes_ ["card", col]] [H.span_ [] sp]

data SetProperty a where
  SetNumber :: SetProperty SetNumber
  SetFill :: SetProperty SetFill
  SetShape :: SetProperty SetShape
  SetColor :: SetProperty SetColor

getProperty :: SetProperty a -> SetCard -> a
getProperty SetNumber = cardNumber
getProperty SetFill = cardFill
getProperty SetShape = cardShape
getProperty SetColor = cardColor

setProperty :: SetProperty a -> SetCard -> a -> SetCard
setProperty SetNumber card n = card{ cardNumber = n }
setProperty SetFill card f = card{ cardFill = f }
setProperty SetShape card s = card{ cardShape = s }
setProperty SetColor card c = card{ cardColor = c }
