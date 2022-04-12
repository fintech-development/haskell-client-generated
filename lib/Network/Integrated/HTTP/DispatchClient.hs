{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Integrated.HTTP.DispatchClient where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Network.HTTP.Client as NH
import Network.Integrated.HTTP.Client (MimeError, MimeResult)
import qualified Network.Integrated.HTTP.Client as Client
import Network.Integrated.HTTP.Core (ClientConfig, Request)
import Network.Integrated.HTTP.MimeTypes

data DispatchClient req res m = DispatchClient
  { dispatchLbs :: forall accept contentType. (Produces req accept, MimeType contentType) => Request req contentType res accept -> m (NH.Response BCL.ByteString),
    dispatchMime :: forall accept contentType. (Produces req accept, MimeUnrender accept res, MimeType contentType) => Request req contentType res accept -> m (MimeResult res),
    dispatchMime' :: forall accept contentType. (Produces req accept, MimeUnrender accept res, MimeType contentType) => Request req contentType res accept -> m (Either MimeError res)
  }

makeDispatchClientIO :: NH.Manager -> ClientConfig -> DispatchClient req res IO
makeDispatchClientIO manager cfg =
  DispatchClient
    { dispatchLbs = Client.dispatchLbs manager cfg,
      dispatchMime = Client.dispatchMime manager cfg,
      dispatchMime' = Client.dispatchMime' manager cfg
    }

makeDispatchClient :: MonadIO m => NH.Manager -> ClientConfig -> DispatchClient req res m
makeDispatchClient manager cfg =
  DispatchClient
    { dispatchLbs = liftIO . Client.dispatchLbs manager cfg,
      dispatchMime = liftIO . Client.dispatchMime manager cfg,
      dispatchMime' = liftIO . Client.dispatchMime' manager cfg
    }
