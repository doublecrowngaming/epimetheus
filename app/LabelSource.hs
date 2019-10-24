{-# LANGUAGE ScopedTypeVariables #-}

module LabelSource (
  LabelSource(..),
  evaluateLabelSource
) where

import           Control.Monad.Catch    (MonadCatch, handleAll)
import           Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString        as BS (readFile)
import qualified Data.ByteString.Char8  as BS8 (pack)
import           Prometheus.Format.Type (LabelValue (..))
import           System.Exit            (exitFailure)
import           System.Log.Logger      (Priority (CRITICAL), logM)
import           System.Process         (readCreateProcess, shell)

data LabelSource =
    LiteralSource String
  | FileLabelSource FilePath
  | CommandLabelSource String deriving Show

evaluateLabelSource :: (MonadCatch m, MonadIO m) => LabelSource -> m LabelValue
evaluateLabelSource = handleAll logAndDie . evaluateLabelSource'
  where
    logAndDie exc = liftIO $ do
      logM "label_evaluator" CRITICAL (show exc)
      exitFailure

evaluateLabelSource' :: MonadIO m => LabelSource -> m LabelValue
evaluateLabelSource' (LiteralSource bs)       = return $ LabelValue . BS8.pack $ bs
evaluateLabelSource' (FileLabelSource path)   = fileSource path
evaluateLabelSource' (CommandLabelSource cmd) = commandSource cmd

fileSource :: MonadIO m => FilePath -> m LabelValue
fileSource path = liftIO (LabelValue <$> BS.readFile path)

commandSource :: MonadIO m => String -> m LabelValue
commandSource cmd = liftIO $ do
  stdout <- readCreateProcess (shell cmd) []

  return $ LabelValue $ BS8.pack stdout
