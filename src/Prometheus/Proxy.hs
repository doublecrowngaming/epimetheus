{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prometheus.Proxy where

import           Control.Exception         (SomeException)
import           Control.Monad.Catch       (MonadThrow, throwM)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.IO.Unlift   (MonadUnliftIO)
import           Data.Conduit
import           Network.HTTP.Client       (HttpException (..),
                                            HttpExceptionContent (..))
import           Network.HTTP.Types.Status (notFound404, ok200)
import           Network.Wai.Conduit
import           Network.Wai.Handler.Warp  hiding (Port)
import           Prelude                   hiding (lines, map)
import           Prometheus.Conduit
import           Prometheus.Format.Type    (PrometheusLine)
import           System.Log.Logger         (Priority (..), logM)


data PrometheusProxy = PrometheusProxy {
  ppPort            :: Port,
  ppSources         :: [PrometheusLocation],
  ppHandleException :: forall i o m. (MonadUnliftIO m, MonadThrow m) => ConduitT i o m () -> ConduitT i o m (),
  ppRewriteRules    :: forall m. Monad m => ConduitT PrometheusLine PrometheusLine m ()
}

prometheusProxy :: PrometheusProxy -> IO ()
prometheusProxy PrometheusProxy{..} =
  run port (\req -> proxy (requestMethod req) (pathInfo req))

  where
    proxy "GET" ["metrics"] respond =
      respond $ responseConduit ok200 [] prometheusSources

    proxy _ _ respond = respond $ responseLBS notFound404 [] ""

    prometheusSources =
      mapM_ (ppHandleException . prometheusSource) ppSources
      .| ppRewriteRules
      .| prometheusRenderer

    port = fromIntegral $ unPort ppPort


onExceptionSkip :: MonadUnliftIO m => ConduitT i o m () -> ConduitT i o m ()
onExceptionSkip = handleC (\(_ :: SomeException) -> return ())

onExceptionPrint :: MonadUnliftIO m => ConduitT i o m () -> ConduitT i o m ()
onExceptionPrint = handleC (\(e :: SomeException) -> liftIO $ print e)

onExceptionLog :: (MonadUnliftIO m, MonadThrow m) => ConduitT i o m () -> ConduitT i o m ()
onExceptionLog = handleC handleUnknownError . handleC handleParseError . handleC handleHttpError
  where
    handleParseError (PrometheusParseError loc badstr _) =
      liftIO $
        logM "Connector.Parser" WARNING ("Invalid line " <> show badstr <> " received from " <> show loc)

    handleHttpError (HttpExceptionRequest req hec) =
      case locationFromRequest req of
        Nothing -> rethrow
        Just loc -> liftIO . logM "Connector.HTTP" ERROR $
                      case hec of
                        ConnectionFailure exc -> "Could not connect to " <> show loc <> ": " <> show exc
                        other -> "HTTP exception from " <> show loc <> ": " <> show other
      where
        rethrow = throwM (HttpExceptionRequest req hec)
    handleHttpError (InvalidUrlException url reason) =
      liftIO $ logM "Connector.HTTP" ERROR ("Invalid URL " <> url <> ": " <> reason)

    handleUnknownError (exc :: SomeException) = do
      liftIO $ logM "Connector" CRITICAL ("Fatal exception while proxying: " <> show exc)
      throwM exc
