{-# LANGUAGE DerivingVia, UndecidableInstances, ViewPatterns, QuasiQuotes, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module StarSet.Card.Permutation
  ( PermutationCard(..)
  , permutationDeck
  , permutationStyles
  , renderPermutationCard
  , identityCard
  , composeCards
  , cardParity
  , Terminator(..)
  , Z3(..)
  , generate
  ) where

import StarSet.Util.JSON

import Data.Proxy
import GHC.TypeNats
import GHC.Generics

import Data.String.Interpolate (i)
import Miso (View, toMisoString, CSS(..), (=:), MisoString)
import qualified Miso.CSS as C
import qualified Miso.Svg as S
import qualified Miso.Svg.Property as P
import qualified Miso.Html.Property as P
import qualified Miso.JSON as Miso
import qualified Data.Aeson as Aeson
import Data.Vector.Fixed (generate, ArityPeano, Peano, toList)
import qualified Data.Vector.Fixed as VF
import Data.Vector.Fixed.Boxed (Vec)
import qualified Data.Set as Set
import Combinatorics
import qualified Data.Vector as V

class Ord terminator => Terminator terminator where
  nullTerminator :: terminator
  composeTerminators :: terminator -> terminator -> terminator
  terminators :: [terminator]
  renderTerminator :: terminator -> MisoString -> (Double, Double) -> [View model action]

instance (KnownNat n, ArityPeano (Peano n), Aeson.FromJSON a) => Aeson.FromJSON (Vec n a) where
  parseJSON (Aeson.Array xs@((== fromIntegral (natVal $ Proxy @n)) . V.length -> True)) = generate . (V.!) <$> mapM Aeson.parseJSON xs
  parseJSON _ = fail "Invalid Vec JSON"

instance (ArityPeano (Peano n), Aeson.ToJSON a) => Aeson.ToJSON (Vec n a) where
  toJSON = Aeson.Array . V.fromList . fmap Aeson.toJSON . toList

data PermutationCard (n :: Nat) terminator = PermutationCard (Vec n Natural) (Vec n terminator)
  deriving (Generic, Generic1)
deriving instance (ArityPeano (Peano n), Eq terminator) => Eq (PermutationCard n terminator)
deriving instance (ArityPeano (Peano n), Ord terminator) => Ord (PermutationCard n terminator)
deriving instance (ArityPeano (Peano n), Show terminator) => Show (PermutationCard n terminator)
deriving via (ViaAeson (PermutationCard n terminator)) instance (KnownNat n, ArityPeano (Peano n), Aeson.FromJSON terminator) => Miso.FromJSON (PermutationCard n terminator)
deriving via (ViaAeson (PermutationCard n terminator)) instance (KnownNat n, ArityPeano (Peano n), Aeson.FromJSON terminator) => Aeson.FromJSON (PermutationCard n terminator)
deriving via (ViaAeson (PermutationCard n terminator)) instance (ArityPeano (Peano n), Aeson.ToJSON terminator) => Miso.ToJSON (PermutationCard n terminator)
deriving via (ViaAeson (PermutationCard n terminator)) instance (ArityPeano (Peano n), Aeson.ToJSON terminator) => Aeson.ToJSON (PermutationCard n terminator)

identityCard :: (ArityPeano (Peano n), Terminator terminator) => PermutationCard n terminator
identityCard = PermutationCard (generate fromIntegral) (VF.replicate nullTerminator)

composeCards :: (ArityPeano (Peano n), Terminator terminator) => PermutationCard n terminator -> PermutationCard n terminator -> PermutationCard n terminator
composeCards (PermutationCard aps ats) (PermutationCard bps bts) = PermutationCard (VF.map (\bp -> aps VF.! fromIntegral bp) bps) (VF.zipWith composeTerminators ats $ VF.map (\bp -> bts VF.! fromIntegral bp) bps)

cardParity :: (ArityPeano (Peano n), Terminator terminator) => PermutationCard n terminator -> Bool
cardParity (PermutationCard ps _) = odd $ inversions $ toList ps where
  inversions [] = 0
  inversions (t:ts) = length (filter (< t) ts) + inversions ts

permutationDeck :: forall n terminator. (KnownNat n, ArityPeano (Peano n), Terminator terminator) => Set.Set (PermutationCard n terminator)
permutationDeck = Set.fromList $ (\ps ts -> PermutationCard (generate (ps !!)) (generate (ts !!))) <$> permuteFast [0..natVal (Proxy @n) - 1] <*> variateRep (fromIntegral $ natVal $ Proxy @n) terminators

permutationStyles :: [CSS]
permutationStyles =
  [ Sheet $ C.sheet_
    [ C.selector_ ".card"
      [ C.width $ C.pct 100
      , C.height $ C.pct 100
      , "-webkit-user-select" =: "none"
      , "-moz-user-select" =: "none"
      , "-ms-user-select" =: "none"
      , "user-select" =: "none"
      ]
    , C.selector_ ".line0"
      [ "--color" =: "#e63946"
      ]
    , C.selector_ ".line1"
      [ "--color" =: "#2d9a4e"
      ]
    , C.selector_ ".line2"
      [ "--color" =: "#7b5ea7"
      ]
    , C.selector_ ".line3"
      [ "--color" =: "#e9c46a"
      ]
    , C.selector_ ".line4"
      [ "--color" =: "#c03db8"
      ]
    , C.selector_ ".line5"
      [ "--color" =: "#0e76e8"
      ]
    , C.selector_ ".perm"
      [ C.stroke "var(--color)"
      , C.fill "transparent"
      , C.strokeWidth "10"
      ]
    , C.selector_ ".bead"
      [ "r" =: "15px"
      , C.strokeWidth "5"
      ]
    , C.selector_ ".bead1"
      [ C.stroke "var(--color)"
      , C.fill "var(--color)"
      ]
    , C.selector_ ".bead2"
      [ C.stroke "var(--color)"
      , C.fill "white"
      ]
    , C.selector_ ".dot-outside"
      [ C.stroke "black"
      , C.fill "transparent"
      , C.strokeWidth "10"
      ]
    , C.selector_ ".dot-center"
      [ C.stroke "transparent"
      ]
    , C.selector_ ".dot-odd"
      [ C.fill "#e63946"
      ]
    , C.selector_ ".dot-even"
      [ C.fill "transparent"
      ]
    ]
  ]

renderPermutationCard' :: forall n terminator model action. (KnownNat n, ArityPeano (Peano n), Terminator terminator) => Bool -> (Double, Double) -> PermutationCard n terminator -> [View model action]
renderPermutationCard' dot (w, h) c@(PermutationCard ps ts) = concat (zipWith3 (\a b t ->
  S.path_
    [ P.d_ $ toMisoString ([i|
    M 0     #{y a}
      #{xa} #{y a}
      #{xb} #{y b}
      #{w}  #{y b}
    |] :: String)
    , P.classes_ ["perm", "line" <> toMisoString (show $ a `mod` 6)]
    ]
  : renderTerminator t ("line" <> toMisoString (show $ a `mod` 6)) (xt, y b)) [(0 :: Natural)..] (toList ps) (toList ts)) ++ (if dot then 
  [ S.circle_ [P.cx_ $ toMisoString $ w / 2, P.cy_ $ toMisoString $ y n, P.r_ $ toMisoString $ hu / 3, P.classes_ ["dot-outside"]]
  , S.circle_ [P.cx_ $ toMisoString $ w / 2, P.cy_ $ toMisoString $ y n, P.r_ $ toMisoString $ hu / 5, P.classes_ ["dot-center", if cardParity c then "dot-odd" else "dot-even"]]
  ] else []) where
  n = natVal $ Proxy @n
  xa = w / 3
  xb = 2 * w / 3
  xt = 5 * w / 6
  hu = h / (if dot then fromIntegral n + 2 else fromIntegral n + 1)
  y t = (fromIntegral t + 1) * hu

renderPermutationCard :: (KnownNat n, ArityPeano (Peano n), Terminator terminator) => Bool -> PermutationCard n terminator -> View model action
renderPermutationCard dot card = let (w, h) = (500, 700) in S.svg_ [P.viewBox_ $ toMisoString ([i|0 0 #{w} #{h}|] :: String)] $ renderPermutationCard' dot (w, h) card

instance Terminator () where
  nullTerminator = ()
  composeTerminators = const
  terminators = [()]
  renderTerminator _ _ _ = []

instance Terminator Bool where
  nullTerminator = False
  composeTerminators = (/=)
  terminators = [False, True]
  renderTerminator False _ _ = []
  renderTerminator True cls (x, y) = [S.circle_ [P.cx_ $ toMisoString x, P.cy_ $ toMisoString y, P.classes_ ["bead", "bead1", cls]]]

data Z3 = None | Filled | Empty
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic)
  deriving (Miso.FromJSON, Miso.ToJSON, Aeson.FromJSON, Aeson.ToJSON) via (ViaAeson Z3)

instance Terminator Z3 where
  nullTerminator = None
  composeTerminators (fromEnum -> a) (fromEnum -> b) = toEnum $ (a + b) `mod` 3
  terminators = [None, Filled, Empty]
  renderTerminator None _ _ = []
  renderTerminator Filled cls (x, y) = [S.circle_ [P.cx_ $ toMisoString x, P.cy_ $ toMisoString y, P.classes_ ["bead", "bead1", cls]]]
  renderTerminator Empty cls (x, y) = [S.circle_ [P.cx_ $ toMisoString x, P.cy_ $ toMisoString y, P.classes_ ["bead", "bead2", cls]]]
