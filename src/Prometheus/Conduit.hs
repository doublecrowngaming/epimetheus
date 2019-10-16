{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Prometheus.Conduit where

import           Control.Monad.Catch          (Exception, MonadThrow, throwM)
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Attoparsec.ByteString   (parseOnly)
import           Data.ByteString.Char8        (ByteString)
import           Data.Conduit
import           Data.Conduit.Binary          (lines)
import           Data.Conduit.Combinators     (map, mapM)
import           Data.Function                ((&))
import           Data.Word                    (Word16)
import           Network.HTTP.Simple
import           Prelude                      hiding (lines, map, mapM)
import           Prometheus.Format.Parse      (parsePrometheusLine)
import           Prometheus.Format.Render     (renderPrometheusLine)
import           Prometheus.Format.Type       (LabelName, LabelValue,
                                               PrometheusLine, addLabel)

data PrometheusParseError = PrometheusParseError ByteString String deriving Show
instance Exception PrometheusParseError

newtype Port = Port { unPort :: Word16 } deriving (Num)

instance Show Port where
  show (Port word16) = "Port " <> show word16

newtype Path = Path ByteString deriving Show

data PrometheusLocation = PrometheusLocation {
  psPort :: Port,
  psPath :: Path
} deriving Show

prometheusSource :: (MonadThrow m, MonadResource m) => PrometheusLocation -> ConduitT i PrometheusLine m ()
prometheusSource PrometheusLocation { psPort = (Port port), psPath = (Path path) } =
  httpSource
    (defaultRequest
      & setRequestPort (fromIntegral port)
      & setRequestPath path)
    getResponseBody .| lines .| mapM parsePrometheus

  where
    parsePrometheus line = case parseOnly parsePrometheusLine line of
      Left err  -> throwM $ PrometheusParseError line err
      Right val -> return val

prometheusRenderer :: Monad m => ConduitT PrometheusLine ByteString m ()
prometheusRenderer = map renderPrometheusLine .| map (<> "\n")

identityRewriter :: Monad m => ConduitT PrometheusLine PrometheusLine m ()
identityRewriter = map id

addLabel :: Monad m => LabelName -> LabelValue -> ConduitT PrometheusLine PrometheusLine m ()
addLabel = (map .) . Prometheus.Format.Type.addLabel
