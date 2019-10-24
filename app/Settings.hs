{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Settings where

import           Data.ByteString.Char8 (pack)
import           Data.Yaml             (FromJSON (..), withObject, (.:))
import           Prometheus.Conduit    (Path (..), Port (..),
                                        PrometheusLocation (..))


newtype JSON a = JSON { unJSON :: a }

data Settings = Settings {
  listenPort :: Port,
  sources    :: [PrometheusLocation]
} deriving Show

instance FromJSON Settings where
  parseJSON = withObject "Settings" $ \obj ->
                Settings
                  <$> (unJSON <$> obj     .: "listen_port")
                  <*> (map unJSON <$> obj .: "prometheus_sources")

instance FromJSON (JSON Port) where
  parseJSON v = JSON . Port <$> parseJSON v

instance FromJSON (JSON Path) where
  parseJSON v = JSON . Path . pack <$> parseJSON v

instance FromJSON (JSON PrometheusLocation) where
  parseJSON v = JSON <$> withObject "Prometheus Location" (
                          \obj -> PrometheusLocation
                                    <$> (unJSON <$> obj .: "port")
                                    <*> (unJSON <$> obj .: "path")
                          ) v
