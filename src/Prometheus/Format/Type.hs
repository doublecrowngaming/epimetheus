{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Prometheus.Format.Type where

import           Data.ByteString
import           Data.Scientific (Scientific)
import           Data.String

newtype LabelName   = LabelName  ByteString deriving (Eq, Show, IsString)
newtype LabelValue  = LabelValue ByteString deriving (Eq, Show, IsString)
newtype MetricName  = MetricName ByteString deriving (Eq, Show, IsString)
newtype MetricTime  = MetricTime ByteString deriving (Eq, Show, IsString)
newtype MetricValue = MetricValue Scientific deriving (Eq, Show, Num, Fractional)

data PrometheusLine =
    Comment ByteString
  | EmptyLine
  | Metric MetricName [(LabelName, LabelValue)] MetricValue (Maybe MetricTime)
  deriving (Eq, Show)

addLabel :: LabelName -> LabelValue -> PrometheusLine -> PrometheusLine
addLabel ln lv (Metric name labels value time) = Metric name ((ln, lv) : labels) value time
addLabel _  _  pl                              = pl

addLabels :: [(LabelName, LabelValue)] -> PrometheusLine -> PrometheusLine
addLabels new (Metric name labels value time) = Metric name (new ++ labels) value time
addLabels _   pl                              = pl
