{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Settings where

import           Control.Applicative    ((<|>))
import           Data.ByteString.Char8  (pack)
import           Data.Yaml              (FromJSON (..), withObject, (.:))
import           LabelSource            (LabelSource (..))
import           Prometheus.Conduit     (Path (..), Port (..),
                                         PrometheusLocation (..))
import           Prometheus.Format.Type (LabelName (..))


newtype JSON a = JSON { unJSON :: a }

data Settings = Settings {
  listenPort :: Port,
  sources    :: [PrometheusLocation],
  labels     :: [LabelInjection]
} deriving Show

data LabelInjection = LabelInjection LabelName LabelSource deriving Show

instance FromJSON Settings where
  parseJSON = withObject "Settings" $ \obj ->
                Settings
                  <$> (unJSON <$> obj     .: "listen_port")
                  <*> (map unJSON <$> obj .: "prometheus_sources")
                  <*> (obj                .: "inject_labels")

instance FromJSON LabelInjection where
  parseJSON = withObject "Label Injection" $ \ obj ->
                LabelInjection
                  <$> (unJSON <$> obj .: "label")
                  <*> (     (FileLabelSource    <$> obj .: "filename")
                        <|> (CommandLabelSource <$> obj .: "command")
                        <|> (LiteralSource      <$> obj .: "literal"))

instance FromJSON (JSON Port) where
  parseJSON v = JSON . Port <$> parseJSON v

instance FromJSON (JSON Path) where
  parseJSON v = JSON . Path . pack <$> parseJSON v

instance FromJSON (JSON LabelName) where
  parseJSON v = JSON . LabelName . pack <$> parseJSON v

instance FromJSON (JSON PrometheusLocation) where
  parseJSON v = JSON <$> withObject "Prometheus Location" (
                          \obj -> PrometheusLocation
                                    <$> (unJSON <$> obj .: "port")
                                    <*> (unJSON <$> obj .: "path")
                          ) v
