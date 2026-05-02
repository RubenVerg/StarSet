{-# LANGUAGE ViewPatterns, OverloadedStrings, DerivingVia #-}

module StarSet.Message
  ( C2SMessage(..)
  , S2CMessage(..)
  ) where

import StarSet.Game
import StarSet.Util.JSON

import Numeric.Natural
import GHC.Generics

import qualified Miso.JSON as Miso
import Miso.String
import Data.Set (Set)

data C2SMessage g
  = C2SGetDeck
  | C2SGetInfo
  | C2SSelection [Card g]
  deriving (Generic)

deriving instance Game g => Eq (C2SMessage g)
deriving instance Game g => Ord (C2SMessage g)
deriving instance Game g => Show (C2SMessage g)
deriving via (ViaAeson (C2SMessage g)) instance Game g => Miso.FromJSON (C2SMessage g)
deriving via (ViaAeson (C2SMessage g)) instance Game g => Miso.ToJSON (C2SMessage g)

data S2CMessage g
  = S2CSetDeck Natural [Card g]
  | S2CInfo { infoStart :: Double, infoCode :: MisoString, infoPlayers :: Natural }
  | S2CGameOver { overTime :: Double, overYours :: Natural, overOthers :: [Natural] }
  | S2CAchieve { achieve :: Set Achievement }
  deriving (Generic)

deriving instance Game g => Eq (S2CMessage g)
deriving instance Game g => Ord (S2CMessage g)
deriving instance Game g => Show (S2CMessage g)
deriving via (ViaAeson (S2CMessage g)) instance Game g => Miso.FromJSON (S2CMessage g)
deriving via (ViaAeson (S2CMessage g)) instance Game g => Miso.ToJSON (S2CMessage g)