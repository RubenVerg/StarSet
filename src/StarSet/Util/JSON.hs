{-# LANGUAGE UndecidableInstances, CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module StarSet.Util.JSON
  ( ViaAeson(..)
  , FromJSON
  , ToJSON
  ) where

import Data.Kind
import Data.Void
import GHC.Generics

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Miso.JSON as Miso
import Miso.String (MisoString, toMisoString, fromMisoString)
import qualified Data.Map as Map
import qualified Data.Vector as V

aesonToMiso :: Aeson.Value -> Miso.Value
aesonToMiso (Aeson.Number num) = Miso.Number $ realToFrac num
aesonToMiso (Aeson.Bool b) = Miso.Bool b
aesonToMiso (Aeson.String t) = Miso.String $ toMisoString t
aesonToMiso (Aeson.Array xs) = Miso.Array $ aesonToMiso <$> V.toList xs
aesonToMiso (Aeson.Object o) = Miso.Object $ Map.map aesonToMiso $ Map.mapKeys (toMisoString . Aeson.toText) $ Aeson.toMap o
aesonToMiso Aeson.Null = Miso.Null

misoToAeson :: Miso.Value -> Aeson.Value
misoToAeson (Miso.Number num) = Aeson.Number $ realToFrac num
misoToAeson (Miso.Bool b) = Aeson.Bool b
misoToAeson (Miso.String t) = Aeson.String $ fromMisoString t
misoToAeson (Miso.Array xs) = Aeson.Array $ V.fromList $ misoToAeson <$> xs
misoToAeson (Miso.Object o) = Aeson.Object $ Aeson.fromMap $ Map.map misoToAeson $ Map.mapKeys (Aeson.fromText . fromMisoString) o
misoToAeson Miso.Null = Aeson.Null

newtype ViaAeson a = ViaAeson a
  deriving (Generic)

instance (Generic a, Aeson.GFromJSON Aeson.Zero (Rep a)) => Aeson.FromJSON (ViaAeson a) where
  parseJSON = fmap ViaAeson . Aeson.genericParseJSON Aeson.defaultOptions

instance (Generic a, Aeson.GFromJSON Aeson.Zero (Rep a)) => Miso.FromJSON (ViaAeson a) where
  parseJSON v = case Aeson.parseEither Aeson.parseJSON (misoToAeson v) of
    Right a -> pure a
    Left err -> fail err

instance (Generic a, Aeson.GToJSON' Aeson.Value Aeson.Zero (Rep a)) => Aeson.ToJSON (ViaAeson a) where
  toJSON (ViaAeson a) = Aeson.genericToJSON Aeson.defaultOptions a

instance (Generic a, Aeson.GToJSON' Aeson.Value Aeson.Zero (Rep a)) => Miso.ToJSON (ViaAeson a) where
  toJSON = aesonToMiso . Aeson.toJSON

type FromJSON :: Type -> Constraint
type FromJSON a = (Miso.FromJSON a, Aeson.FromJSON a)

type ToJSON :: Type -> Constraint
type ToJSON a = (Miso.ToJSON a, Aeson.ToJSON a)

instance Miso.FromJSON Void where parseJSON = const $ fail "parseJSON @Void"

instance Miso.ToJSON Void where toJSON = absurd

#if defined(wasm32_HOST_ARCH)
instance Aeson.FromJSON MisoString where
  parseJSON (Aeson.String t) = pure $ toMisoString t
  parseJSON _ = fail "Invalid JSON type parsing a MisoString"

instance Aeson.ToJSON MisoString where
  toJSON = Aeson.String . fromMisoString
#endif
