{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

module StarSet.Message
  ( C2SMessage(..)
  , S2CMessage(..)
  ) where

import StarSet.Game

import Numeric.Natural
import GHC.Generics

import Miso.JSON
import Miso.String
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data C2SMessage g
  = C2SGetDeck
  | C2SGetInfo
  | C2SSelection [Card g]

deriving instance Game g => Eq (C2SMessage g)
deriving instance Game g => Ord (C2SMessage g)
deriving instance Game g => Show (C2SMessage g)

instance Game g => FromJSON (C2SMessage g) where
  parseJSON (Object (Map.lookup "type" -> Just (String "get-deck"))) = pure C2SGetDeck
  parseJSON (Object (Map.lookup "type" -> Just (String "get-info"))) = pure C2SGetInfo
  parseJSON (Object ((,) <$> Map.lookup "type" <*> Map.lookup "selected" -> (Just (String "selection"), Just (Array vals)))) = C2SSelection <$> mapM parseJSON vals
  parseJSON _ = fail "Invalid client-to-server message"
instance Game g => ToJSON (C2SMessage g) where
  toJSON C2SGetDeck = Object $ Map.fromList [("type", "get-deck")]
  toJSON C2SGetInfo = Object $ Map.fromList [("type", "get-info")]
  toJSON (C2SSelection cards) = Object $ Map.fromList [("type", "selection"), ("selected", Array $ toJSON <$> cards)]

data S2CMessage g
  = S2CSetDeck Natural [Card g]
  | S2CInfo { infoStart :: Double, infoCode :: MisoString }
  | S2CGameOver { overTime :: Double, overYours :: Natural, overOthers :: [Natural] }
  | S2CAchieve { achieve :: Set Achievement }
  deriving (Generic)

deriving instance Game g => Eq (S2CMessage g)
deriving instance Game g => Ord (S2CMessage g)
deriving instance Game g => Show (S2CMessage g)

instance Game g => FromJSON (S2CMessage g) where
  parseJSON (Object ((,,) <$> Map.lookup "type" <*> Map.lookup "deck" <*> Map.lookup "showing"
    -> (Just (String "set-deck"), Just (Array vals), Just showing))) = S2CSetDeck <$> parseJSON showing <*> mapM parseJSON vals
  parseJSON (Object ((,,) <$> Map.lookup "type" <*> Map.lookup "start" <*> Map.lookup "code"
    -> (Just (String "info"), Just (Number start), Just (String code)))) = pure $ S2CInfo start code
  parseJSON (Object ((,,,) <$> Map.lookup "type" <*> Map.lookup "time" <*> Map.lookup "yours" <*> Map.lookup "others"
    -> (Just (String "game-over"), Just (Number time), Just yours, Just others))) = S2CGameOver time <$> parseJSON yours <*> parseJSON others
  parseJSON (Object ((,) <$> Map.lookup "type" <*> Map.lookup "achievements"
    -> (Just (String "achieve"), Just (Array achs)))) = S2CAchieve . Set.fromList <$> mapM parseJSON achs
  parseJSON _ = fail "Invalid server-to-client message"
instance Game g => ToJSON (S2CMessage g) where
  toJSON (S2CSetDeck showing cards) = Object $ Map.fromList [("type", "set-deck"), ("deck", Array $ toJSON <$> cards), ("showing", toJSON showing)]
  toJSON (S2CInfo start code) = Object $ Map.fromList [("type", "info"), ("start", Number start), ("code", String code)]
  toJSON (S2CGameOver time yours others) = Object $ Map.fromList [("type", "game-over"), ("time", Number time), ("yours", toJSON yours), ("others", toJSON others)]
  toJSON (S2CAchieve achs) = Object $ Map.fromList [("type", "achieve"), ("achievements", Array $ toJSON <$> Set.toList achs)]
