{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Prometheus.Format.Render (
  renderPrometheusLine
) where

import           Data.ByteString.Char8  (ByteString, intercalate, pack)
import           Prometheus.Format.Type

class Render a where
  render :: a -> ByteString

renderPrometheusLine :: PrometheusLine -> ByteString
renderPrometheusLine = render

instance Render LabelName where
  render (LabelName n) = n

instance Render LabelValue where
  render (LabelValue v) = v

instance Render MetricName where
  render (MetricName n) = n

instance Render MetricTime where
  render (MetricTime t) = t

instance Render MetricValue where
  render (MetricValue v) = pack . show $ v

instance Render (LabelName, LabelValue) where
  render (name, val) = render name <> "=\"" <> render val <> "\""

instance Render [(LabelName, LabelValue)] where
  render []    = ""
  render pairs = "{" <> intercalate "," (map render pairs) <> "}"

instance Render PrometheusLine where
  render EmptyLine                              = ""
  render (Comment comment)                      = "#" <> comment
  render (Metric name labels value Nothing)     = render name <> render labels <> " " <> render value
  render (Metric name labels value (Just time)) = render (Metric name labels value Nothing) <> " " <> render time
