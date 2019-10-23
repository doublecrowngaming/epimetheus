{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prometheus.Proxy where

import           Control.Exception         (SomeException)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.IO.Unlift   (MonadUnliftIO)
import           Data.Conduit
import           Network.HTTP.Types.Status (notFound404, ok200)
import           Network.Wai.Conduit
import           Network.Wai.Handler.Warp  hiding (Port)
import           Prelude                   hiding (lines, map)
import           Prometheus.Conduit


data PrometheusProxy = PrometheusProxy {
  ppPort            :: Port,
  ppSources         :: [PrometheusLocation],
  ppHandleException :: forall i o m. MonadUnliftIO m => ConduitT i o m () -> ConduitT i o m ()
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
      .| prometheusRenderer

    port = fromIntegral $ unPort ppPort


onExceptionSkip :: MonadUnliftIO m => ConduitT i o m () -> ConduitT i o m ()
onExceptionSkip = handleC (\(_ :: SomeException) -> return ())

onExceptionPrint :: MonadUnliftIO m => ConduitT i o m () -> ConduitT i o m ()
onExceptionPrint = handleC (\(e :: SomeException) -> liftIO $ print e)
