{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Network.Integrated.HTTP.Client where

import Control.Exception.Safe (Exception)
import qualified Control.Exception.Safe as E
import qualified Control.Monad as P
import qualified Control.Monad.IO.Class as P
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BCL
import Data.Function ((&))
import Data.Monoid ((<>))
import qualified Data.Proxy as P (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Exts (IsString (..))
import qualified Network.HTTP.Client as NH
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Types as NH
import Network.Integrated.HTTP.Core
import Network.Integrated.HTTP.Logging
import Network.Integrated.HTTP.MimeTypes
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

-- * Dispatch

-- ** Lbs

-- | send a request returning the raw http response
dispatchLbs ::
  (Produces req accept, MimeType contentType) =>
  -- | http-client Connection manager
  NH.Manager ->
  -- | config
  ClientConfig ->
  -- | request
  Request req contentType res accept ->
  -- | response
  IO (NH.Response BCL.ByteString)
dispatchLbs manager config request = do
  initReq <- _toInitRequest config request
  dispatchInitUnsafe manager config initReq

-- ** Mime

-- | pair of decoded http body and http response
data MimeResult res = MimeResult
  { -- | decoded http body
    mimeResult :: Either MimeError res,
    -- | http response
    mimeResultResponse :: NH.Response BCL.ByteString
  }
  deriving (Show, Functor, Foldable, Traversable)

-- | pair of unrender/parser error and http response
data MimeError = MimeError
  { -- | unrender/parser error
    mimeError :: String,
    -- | http response
    mimeErrorResponse :: NH.Response BCL.ByteString
  }
  deriving (Show, Exception)

-- | send a request returning the 'MimeResult'
dispatchMime ::
  forall req contentType res accept.
  (Produces req accept, MimeUnrender accept res, MimeType contentType) =>
  -- | http-client Connection manager
  NH.Manager ->
  -- | config
  ClientConfig ->
  -- | request
  Request req contentType res accept ->
  -- | response
  IO (MimeResult res)
dispatchMime manager config request = do
  httpResponse <- dispatchLbs manager config request
  let statusCode = NH.statusCode . NH.responseStatus $ httpResponse
  parsedResult <-
    runConfigLogWithExceptions "Client" config $
      do
        if statusCode >= 400 && statusCode < 600
          then do
            let s = "error statusCode: " ++ show statusCode
            _log "Client" levelError (T.pack s)
            pure (Left (MimeError s httpResponse))
          else case mimeUnrender (P.Proxy :: P.Proxy accept) (NH.responseBody httpResponse) of
            Left s -> do
              _log "Client" levelError (T.pack s)
              pure (Left (MimeError s httpResponse))
            Right r -> pure (Right r)
  return (MimeResult parsedResult httpResponse)

-- | like 'dispatchMime', but only returns the decoded http body
dispatchMime' ::
  (Produces req accept, MimeUnrender accept res, MimeType contentType) =>
  -- | http-client Connection manager
  NH.Manager ->
  -- | config
  ClientConfig ->
  -- | request
  Request req contentType res accept ->
  -- | response
  IO (Either MimeError res)
dispatchMime' manager config request = do
  MimeResult parsedResult _ <- dispatchMime manager config request
  return parsedResult

-- ** Unsafe

-- | like 'dispatchReqLbs', but does not validate the operation is a 'Producer' of the "accept" 'MimeType'.  (Useful if the server's response is undocumented)
dispatchLbsUnsafe ::
  (MimeType accept, MimeType contentType) =>
  -- | http-client Connection manager
  NH.Manager ->
  -- | config
  ClientConfig ->
  -- | request
  Request req contentType res accept ->
  -- | response
  IO (NH.Response BCL.ByteString)
dispatchLbsUnsafe manager config request = do
  initReq <- _toInitRequest config request
  dispatchInitUnsafe manager config initReq

-- | dispatch an InitRequest
dispatchInitUnsafe ::
  -- | http-client Connection manager
  NH.Manager ->
  -- | config
  ClientConfig ->
  -- | init request
  InitRequest req contentType res accept ->
  -- | response
  IO (NH.Response BCL.ByteString)
dispatchInitUnsafe manager config (InitRequest req) = do
  runConfigLogWithExceptions src config $
    do
      _log src levelInfo requestLogMsg
      _log src levelDebug requestDbgLogMsg
      res <- P.liftIO $ NH.httpLbs req manager
      _log src levelInfo (responseLogMsg res)
      _log src levelDebug ((T.pack . show) res)
      return res
  where
    src = "Client"
    endpoint =
      T.pack $
        BC.unpack $
          NH.method req <> " " <> NH.host req <> NH.path req <> NH.queryString req
    requestLogMsg = "REQ:" <> endpoint
    requestDbgLogMsg =
      "Headers=" <> (T.pack . show) (NH.requestHeaders req) <> " Body="
        <> ( case NH.requestBody req of
               NH.RequestBodyLBS xs -> T.decodeUtf8 (BL.toStrict xs)
               _ -> "<RequestBody>"
           )
    responseStatusCode = (T.pack . show) . NH.statusCode . NH.responseStatus
    responseLogMsg res =
      "RES:statusCode=" <> responseStatusCode res <> " (" <> endpoint <> ")"

-- * InitRequest

-- | wraps an http-client 'Request' with request/response type parameters
newtype InitRequest req contentType res accept = InitRequest
  { unInitRequest :: NH.Request
  }
  deriving (Show)

-- |  Build an http-client 'Request' record from the supplied config and request
_toInitRequest ::
  (MimeType accept, MimeType contentType) =>
  -- | config
  ClientConfig ->
  -- | request
  Request req contentType res accept ->
  -- | initialized request
  IO (InitRequest req contentType res accept)
_toInitRequest config req0 =
  runConfigLogWithExceptions "Client" config $ do
    parsedReq <- P.liftIO $ NH.parseRequest $ BCL.unpack $ BCL.append (configHost config) (BCL.concat (rUrlPath req0))
    req1 <- P.liftIO $ _applyAuthMethods req0 config
    P.when
      (configValidateAuthMethods config && (not . null . rAuthTypes) req1)
      (E.throw $ AuthMethodException $ "AuthMethod not configured: " <> (show . head . rAuthTypes) req1)
    let req2 = req1 & _setContentTypeHeader & _setAcceptHeader
        params = rParams req2
        reqHeaders = ("User-Agent", WH.toHeader (configUserAgent config)) : paramsHeaders params
        reqQuery =
          let query = paramsQuery params
              queryExtraUnreserved = configQueryExtraUnreserved config
           in if B.null queryExtraUnreserved
                then NH.renderQuery True query
                else NH.renderQueryPartialEscape True (toPartialEscapeQuery queryExtraUnreserved query)
        pReq =
          parsedReq
            { NH.method = rMethod req2,
              NH.requestHeaders = reqHeaders,
              NH.queryString = reqQuery
            }
    outReq <- case paramsBody params of
      ParamBodyNone -> pure (pReq {NH.requestBody = mempty})
      ParamBodyB bs -> pure (pReq {NH.requestBody = NH.RequestBodyBS bs})
      ParamBodyBL bl -> pure (pReq {NH.requestBody = NH.RequestBodyLBS bl})
      ParamBodyFormUrlEncoded form -> pure (pReq {NH.requestBody = NH.RequestBodyLBS (WH.urlEncodeForm form)})
      ParamBodyMultipartFormData parts -> NH.formDataBody parts pReq

    pure (InitRequest outReq)

-- | modify the underlying Request
modifyInitRequest :: InitRequest req contentType res accept -> (NH.Request -> NH.Request) -> InitRequest req contentType res accept
modifyInitRequest (InitRequest req) f = InitRequest (f req)

-- | modify the underlying Request (monadic)
modifyInitRequestM :: Monad m => InitRequest req contentType res accept -> (NH.Request -> m NH.Request) -> m (InitRequest req contentType res accept)
modifyInitRequestM (InitRequest req) f = fmap InitRequest (f req)

-- ** Logging

-- | Run a block using the configured logger instance
runConfigLog ::
  P.MonadIO m =>
  ClientConfig ->
  LogExec m a
runConfigLog config = configLogExecWithContext config (configLogContext config)

-- | Run a block using the configured logger instance (logs exceptions)
runConfigLogWithExceptions ::
  (E.MonadCatch m, P.MonadIO m) =>
  T.Text ->
  ClientConfig ->
  LogExec m a
runConfigLogWithExceptions src config = runConfigLog config . logExceptions src
