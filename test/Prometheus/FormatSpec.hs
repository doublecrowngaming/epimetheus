{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -Wno-orphans            #-}

module Prometheus.FormatSpec (spec) where

import           Data.Attoparsec.ByteString
import           Data.ByteString.Char8      (all, notElem, null, unpack)
import           Data.Char
import           Prelude                    hiding (all, notElem, null)
import           Prometheus.Format.Parse
import           Prometheus.Format.Render
import           Prometheus.Format.Type
import           Test.QuickCheck.Instances  ()

import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary LabelName where
  arbitrary = LabelName <$> arbitrary
                `suchThat` all (\c -> isAscii c && (isDigit c || isLetter c || c == '_'))
                `suchThat` (not . null)

instance Arbitrary LabelValue where
  arbitrary = LabelValue <$> arbitrary `suchThat` notElem '"'

instance Arbitrary MetricName where
  arbitrary = MetricName <$> arbitrary
                `suchThat` all (\c -> isAscii c && (isDigit c || isLetter c || c == '_' || c == ':' ))
                `suchThat` (not . null)

instance Arbitrary MetricTime where
  arbitrary = MetricTime <$> arbitrary
                `suchThat` all (not . isSpace)
                `suchThat` (not . null)

deriving instance Arbitrary MetricValue

instance Arbitrary PrometheusLine where
  arbitrary = oneof [
                Comment <$> arbitrary,
                pure EmptyLine,
                Metric <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
              ]

spec :: Spec
spec = do
  describe "renderer" $
    it "produces parsable output" $ property $
      \pl -> counterexample (unpack $ renderPrometheusLine pl) $
              parseOnly parsePrometheusLine (renderPrometheusLine pl) === Right pl

  describe "parser" $ do
    it "parses node_network_transmit_packets_total{device=\"lo0\"} 6.147174e+06" $
      parseOnly parsePrometheusLine "node_network_transmit_packets_total{device=\"lo0\"} 6.147174e+06"
        `shouldBe` (Right $ Metric "node_network_transmit_packets_total" [("device", "lo0")] 6.147174e+06 Nothing)

    it "parses # Foo" $
      parseOnly parsePrometheusLine " # FOO" `shouldBe` (Right $ Comment " FOO")

    it "parses empty lines" $
      parseOnly parsePrometheusLine "" `shouldBe` Right EmptyLine
