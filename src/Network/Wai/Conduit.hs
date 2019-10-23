{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Network.Wai.Conduit (
  responseConduit,
  module Network.Wai
) where

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Binary.Builder          (fromByteString)
import           Data.ByteString              (ByteString)
import           Data.Conduit
import           Network.HTTP.Types           (ResponseHeaders, Status)
import           Network.Wai
import           Prelude                      hiding (lines, map, mapM)

responseConduit :: Status -> ResponseHeaders -> ConduitT () ByteString (ResourceT IO) () -> Response
responseConduit status headers conduit =
  responseStream status headers $ \write flush -> do
    let streamingWrite =
          await >>= \case
            Nothing    -> liftIO flush
            Just datum -> liftIO (write (fromByteString datum)) >> streamingWrite

    runResourceT . runConduit $ conduit .| streamingWrite
