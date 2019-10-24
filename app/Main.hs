{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.Functor              ((<&>))
import           Data.Yaml                 (decodeFileThrow)
import           LabelSource               (evaluateLabelSource)
import           Options.Applicative
import           Prometheus.Conduit        (addLabels)
import           Prometheus.Proxy          (PrometheusProxy (..),
                                            onExceptionLog, prometheusProxy)
import           Settings
import           System.IO                 (stderr)
import           System.Log                (Priority (..))
import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (streamHandler)
import           System.Log.Handler.Syslog (Facility (DAEMON), Option (PID),
                                            openlog)
import           System.Log.Logger


data LoggingMode = Console | Syslog

data Parameters = Parameters {
  loggingMode :: LoggingMode,
  configFile  :: FilePath
}

parameters :: Parser Parameters
parameters = Parameters <$> metrics <*> config

  where
    config  = argument str (metavar "CONFIG")
    metrics = (\b -> if b then Syslog else Console) <$>
                switch (
                    long "syslog"
                  <> help "Send log output to syslog"
                )

main :: IO ()
main = do
  Parameters{..} <- customExecParser p (info (parameters <**> helper) fullDesc)
  settings       <- decodeFileThrow configFile

  case loggingMode of
    Console -> streamHandler stderr DEBUG <&> consoleFormatter >>= configureLogging
    Syslog  -> openlog "epimetheus" [PID] DAEMON DEBUG >>= configureLogging

  labelInjections <- mapM evaluateLabelInjection (labels settings)

  startProxy settings labelInjections

  where
    p = prefs (showHelpOnEmpty <> showHelpOnError)

    startProxy Settings{..} injections =
      prometheusProxy PrometheusProxy {
        ppPort            = listenPort,
        ppSources         = sources,
        ppHandleException = onExceptionLog,
        ppRewriteRules    = addLabels injections
      }

    consoleFormatter h = setFormatter h (simpleLogFormatter "[$time : $loggername : $prio] $msg")

    configureLogging handler = do
      updateGlobalLogger rootLoggerName (setHandlers [handler])
      updateGlobalLogger rootLoggerName (setLevel DEBUG)

    evaluateLabelInjection (LabelInjection name source) = do
      labelValue <- evaluateLabelSource source

      logM "main.relabel" DEBUG ("Label injection rule: " <> show name <> " => " <> show labelValue)

      return (name, labelValue)
