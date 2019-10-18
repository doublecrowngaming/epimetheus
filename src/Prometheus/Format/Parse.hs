{-# LANGUAGE OverloadedStrings #-}

module Prometheus.Format.Parse (
  parsePrometheusLine
) where

import           Control.Applicative              (optional, (<|>))
import           Data.Attoparsec.ByteString.Char8
import           Data.Functor                     (($>))
import           Prelude                          hiding (takeWhile)
import           Prometheus.Format.Type           (LabelName (..),
                                                   LabelValue (..),
                                                   MetricName (..),
                                                   MetricTime (..),
                                                   MetricValue (..),
                                                   PrometheusLine (..))

whitespace :: Parser ()
whitespace = skipWhile isSpace

ignoreWhitespace :: Parser a -> Parser a
ignoreWhitespace p = whitespace *> p <* whitespace

parsePrometheusLine :: Parser PrometheusLine
parsePrometheusLine =
  ignoreWhitespace prometheusExpression
  where
    prometheusExpression = emptyLine <|> comment <|> metric
    emptyLine            = endOfInput $> EmptyLine
    comment              = char '#' *> (Comment <$> takeByteString)
    metric               = Metric
                            <$> metricName
                            <*> ignoreWhitespace (labelSection <|> pure [])
                            <*> ignoreWhitespace metricValue
                            <*> ignoreWhitespace (optional metricTime)
    labelSection         = "{" *> labels <* "}"
    labels               = (:) <$> ignoreWhitespace labelPair <*> (char ',' *> labels <|> pure [])
    labelName            = LabelName <$> takeWhile (inClass "[a-zA-Z_][a-zA-Z0-9_]")
    labelValue           = LabelValue <$> takeTill (== '"')
    labelPair            = (,) <$> (labelName <* ignoreWhitespace "=") <*> (char '"' *> labelValue <* char '"')
    metricName           = MetricName <$> takeWhile1 (inClass "[a-zA-Z_:][a-zA-Z0-9_:]")
    metricTime           = MetricTime <$> takeWhile1 (not . isSpace)
    metricValue          = MetricValue <$> scientific
