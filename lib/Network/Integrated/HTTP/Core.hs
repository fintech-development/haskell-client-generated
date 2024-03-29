{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Network.Integrated.HTTP.Core where

import Control.Applicative (Alternative, (<|>))
import qualified Control.Arrow as P (left)
import qualified Control.DeepSeq as NF
import qualified Control.Exception.Safe as E
import Control.Monad.Fail (MonadFail)
import qualified Data.Aeson as A
import qualified Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.Lazy as BL64
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Data.CaseInsensitive as CI
import qualified Data.Data as P (Data, TypeRep, Typeable, typeRep)
import Data.Foldable (foldlM)
import qualified Data.Foldable as P
import Data.Function ((&))
import qualified Data.Ix as P
import qualified Data.Kind as K (Type)
import qualified Data.Maybe as P
import Data.Monoid ((<>))
import qualified Data.Proxy as P (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Time as TI
import qualified Data.Time.ISO8601 as TI
import qualified GHC.Base as P (Alternative)
import qualified Lens.Micro as L
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Types as NH
import Network.Integrated.HTTP.Logging
import Network.Integrated.HTTP.MimeTypes
import qualified Text.Printf as T
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH
import Prelude (Bool (..), Char, Functor, IO, Maybe (..), Monad, String, fmap, maybe, mempty, pure, return, show, ($), (&&), (.), (<$>), (<*>), (||))
import qualified Prelude as P

-- * ClientConfig

-- |
data ClientConfig = ClientConfig
  { -- | host supplied in the Request
    configHost :: BCL.ByteString,
    -- | user-agent supplied in the Request
    configUserAgent :: Text,
    -- | Run a block using a Logger instance
    configLogExecWithContext :: LogExecWithContext,
    -- | Configures the logger
    configLogContext :: LogContext,
    -- | List of configured auth methods
    configAuthMethods :: [AnyAuthMethod],
    -- | throw exceptions if auth methods are not configured
    configValidateAuthMethods :: Bool,
    -- | Configures additional querystring characters which must not be URI encoded, e.g. '+' or ':'
    configQueryExtraUnreserved :: B.ByteString
  }

-- | display the config
instance P.Show ClientConfig where
  show c =
    T.printf
      "{ configHost = %v, configUserAgent = %v, ..}"
      (show (configHost c))
      (show (configUserAgent c))

-- | constructs a default ClientConfig
--
-- configHost:
--
-- @https://api.sandbox.equifax.com/business/prescreen-of-one/v1@
--
-- configUserAgent:
--
-- @"the-equifax-prescreen.cabal/0.1.0.0"@
newConfig :: BCL.ByteString -> IO ClientConfig
newConfig host = do
  logCxt <- initLogContext
  return $
    ClientConfig
      { configHost = host,
        configUserAgent = "gauss/0.1.0.0",
        configLogExecWithContext = runDefaultLogExecWithContext,
        configLogContext = logCxt,
        configAuthMethods = [],
        configValidateAuthMethods = True,
        configQueryExtraUnreserved = ""
      }

-- | updates config use AuthMethod on matching requests
addAuthMethod :: AuthMethod auth => ClientConfig -> auth -> ClientConfig
addAuthMethod config@ClientConfig {configAuthMethods = as} a =
  config {configAuthMethods = AnyAuthMethod a : as}

-- | updates the config to use stdout logging
withStdoutLogging :: ClientConfig -> IO ClientConfig
withStdoutLogging p = do
  logCxt <- stdoutLoggingContext (configLogContext p)
  return $ p {configLogExecWithContext = stdoutLoggingExec, configLogContext = logCxt}

-- | updates the config to use stderr logging
withStderrLogging :: ClientConfig -> IO ClientConfig
withStderrLogging p = do
  logCxt <- stderrLoggingContext (configLogContext p)
  return $ p {configLogExecWithContext = stderrLoggingExec, configLogContext = logCxt}

-- | updates the config to disable logging
withNoLogging :: ClientConfig -> ClientConfig
withNoLogging p = p {configLogExecWithContext = runNullLogExec}

-- * Request

-- | Represents a request.
--
--   Type Variables:
--
--   * req - request operation
--   * contentType - 'MimeType' associated with request body
--   * res - response model
--   * accept - 'MimeType' associated with response body
data Request req contentType res accept = Request
  { -- | Method of Request
    rMethod :: NH.Method,
    -- | Endpoint of Request
    rUrlPath :: [BCL.ByteString],
    -- | params of Request
    rParams :: Params,
    -- | types of auth methods
    rAuthTypes :: [P.TypeRep]
  }
  deriving (P.Show)

-- | 'rMethod' Lens
rMethodL :: Lens_' (Request req contentType res accept) NH.Method
rMethodL f Request {..} = (\rMethod -> Request {rMethod, ..}) <$> f rMethod
{-# INLINE rMethodL #-}

-- | 'rUrlPath' Lens
rUrlPathL :: Lens_' (Request req contentType res accept) [BCL.ByteString]
rUrlPathL f Request {..} = (\rUrlPath -> Request {rUrlPath, ..}) <$> f rUrlPath
{-# INLINE rUrlPathL #-}

-- | 'rParams' Lens
rParamsL :: Lens_' (Request req contentType res accept) Params
rParamsL f Request {..} = (\rParams -> Request {rParams, ..}) <$> f rParams
{-# INLINE rParamsL #-}

-- | 'rParams' Lens
rAuthTypesL :: Lens_' (Request req contentType res accept) [P.TypeRep]
rAuthTypesL f Request {..} = (\rAuthTypes -> Request {rAuthTypes, ..}) <$> f rAuthTypes
{-# INLINE rAuthTypesL #-}

-- * HasBodyParam

-- | Designates the body parameter of a request
class HasBodyParam req param where
  setBodyParam :: forall contentType res accept. (Consumes req contentType, MimeRender contentType param) => Request req contentType res accept -> param -> Request req contentType res accept
  setBodyParam req xs =
    req `_setBodyLBS` mimeRender (P.Proxy :: P.Proxy contentType) xs & _setContentTypeHeader

-- * HasOptionalParam

-- | Designates the optional parameters of a request
class HasOptionalParam req param where
  {-# MINIMAL applyOptionalParam | (-&-) #-}

  -- | Apply an optional parameter to a request
  applyOptionalParam :: Request req contentType res accept -> param -> Request req contentType res accept
  applyOptionalParam = (-&-)
  {-# INLINE applyOptionalParam #-}

  -- | infix operator \/ alias for 'addOptionalParam'
  (-&-) :: Request req contentType res accept -> param -> Request req contentType res accept
  (-&-) = applyOptionalParam
  {-# INLINE (-&-) #-}

infixl 2 -&-

-- | Request Params
data Params = Params
  { paramsQuery :: NH.Query,
    paramsHeaders :: NH.RequestHeaders,
    paramsBody :: ParamBody
  }
  deriving (P.Show)

-- | 'paramsQuery' Lens
paramsQueryL :: Lens_' Params NH.Query
paramsQueryL f Params {..} = (\paramsQuery -> Params {paramsQuery, ..}) <$> f paramsQuery
{-# INLINE paramsQueryL #-}

-- | 'paramsHeaders' Lens
paramsHeadersL :: Lens_' Params NH.RequestHeaders
paramsHeadersL f Params {..} = (\paramsHeaders -> Params {paramsHeaders, ..}) <$> f paramsHeaders
{-# INLINE paramsHeadersL #-}

-- | 'paramsBody' Lens
paramsBodyL :: Lens_' Params ParamBody
paramsBodyL f Params {..} = (\paramsBody -> Params {paramsBody, ..}) <$> f paramsBody
{-# INLINE paramsBodyL #-}

-- | Request Body
data ParamBody
  = ParamBodyNone
  | ParamBodyB B.ByteString
  | ParamBodyBL BL.ByteString
  | ParamBodyFormUrlEncoded WH.Form
  | ParamBodyMultipartFormData [NH.Part]
  deriving (P.Show)

-- ** Request Utils

_mkRequest ::
  -- | Method
  NH.Method ->
  -- | Endpoint
  [BCL.ByteString] ->
  -- | req: Request Type, res: Response Type
  Request req contentType res accept
_mkRequest m u = Request m u _mkParams []

_mkParams :: Params
_mkParams = Params [] [] ParamBodyNone

setHeader ::
  Request req contentType res accept ->
  [NH.Header] ->
  Request req contentType res accept
setHeader req header =
  req `removeHeader` P.fmap P.fst header
    & (`addHeader` header)

addHeader ::
  Request req contentType res accept ->
  [NH.Header] ->
  Request req contentType res accept
addHeader req header = L.over (rParamsL . paramsHeadersL) (header P.++) req

removeHeader :: Request req contentType res accept -> [NH.HeaderName] -> Request req contentType res accept
removeHeader req header =
  req
    & L.over
      (rParamsL . paramsHeadersL)
      (P.filter (\h -> cifst h `P.notElem` P.fmap CI.mk header))
  where
    cifst = CI.mk . P.fst

_setContentTypeHeader :: forall req contentType res accept. MimeType contentType => Request req contentType res accept -> Request req contentType res accept
_setContentTypeHeader req =
  case mimeType (P.Proxy :: P.Proxy contentType) of
    Just m -> req `setHeader` [("content-type", BC.pack $ P.show m)]
    Nothing -> req `removeHeader` ["content-type"]

_setAcceptHeader :: forall req contentType res accept. MimeType accept => Request req contentType res accept -> Request req contentType res accept
_setAcceptHeader req =
  case mimeType (P.Proxy :: P.Proxy accept) of
    Just m -> req `setHeader` [("accept", BC.pack $ P.show m)]
    Nothing -> req `removeHeader` ["accept"]

setQuery ::
  Request req contentType res accept ->
  [NH.QueryItem] ->
  Request req contentType res accept
setQuery req query =
  req
    & L.over
      (rParamsL . paramsQueryL)
      (P.filter (\q -> cifst q `P.notElem` P.fmap cifst query))
    & (`addQuery` query)
  where
    cifst = CI.mk . P.fst

addQuery ::
  Request req contentType res accept ->
  [NH.QueryItem] ->
  Request req contentType res accept
addQuery req query = req & L.over (rParamsL . paramsQueryL) (query P.++)

addForm :: Request req contentType res accept -> WH.Form -> Request req contentType res accept
addForm req newform =
  let form = case paramsBody (rParams req) of
        ParamBodyFormUrlEncoded _form -> _form
        _ -> mempty
   in req & L.set (rParamsL . paramsBodyL) (ParamBodyFormUrlEncoded (newform <> form))

_addMultiFormPart :: Request req contentType res accept -> NH.Part -> Request req contentType res accept
_addMultiFormPart req newpart =
  let parts = case paramsBody (rParams req) of
        ParamBodyMultipartFormData _parts -> _parts
        _ -> []
   in req & L.set (rParamsL . paramsBodyL) (ParamBodyMultipartFormData (newpart : parts))

_setBodyBS :: Request req contentType res accept -> B.ByteString -> Request req contentType res accept
_setBodyBS req body =
  req & L.set (rParamsL . paramsBodyL) (ParamBodyB body)

_setBodyLBS :: Request req contentType res accept -> BL.ByteString -> Request req contentType res accept
_setBodyLBS req body =
  req & L.set (rParamsL . paramsBodyL) (ParamBodyBL body)

_hasAuthType :: AuthMethod authMethod => Request req contentType res accept -> P.Proxy authMethod -> Request req contentType res accept
_hasAuthType req proxy =
  req & L.over rAuthTypesL (P.typeRep proxy :)

-- ** Params Utils

toPath ::
  WH.ToHttpApiData a =>
  a ->
  BCL.ByteString
toPath = BB.toLazyByteString . WH.toEncodedUrlPiece

toHeader :: WH.ToHttpApiData a => (NH.HeaderName, a) -> [NH.Header]
toHeader x = [fmap WH.toHeader x]

toForm :: WH.ToHttpApiData v => (BC.ByteString, v) -> WH.Form
toForm (k, v) = WH.toForm [(BC.unpack k, v)]

toQuery :: WH.ToHttpApiData a => (BC.ByteString, Maybe a) -> [NH.QueryItem]
toQuery x = [(fmap . fmap) toQueryParam x]
  where
    toQueryParam = T.encodeUtf8 . WH.toQueryParam

toPartialEscapeQuery :: B.ByteString -> NH.Query -> NH.PartialEscapeQuery
toPartialEscapeQuery extraUnreserved = fmap (Data.Bifunctor.second (maybe [] go))
  where
    go :: B.ByteString -> [NH.EscapeItem]
    go v =
      v & B.groupBy (\a b -> a `B.notElem` extraUnreserved && b `B.notElem` extraUnreserved)
        & fmap
          ( \xs ->
              if B.null xs || (B.head xs `B.elem` extraUnreserved) then NH.QN xs else NH.QE xs -- Encoded
          )

-- *** OpenAPI `CollectionFormat` Utils

-- | Determines the format of the array if type array is used.
data CollectionFormat
  = -- | CSV format for multiple parameters.
    CommaSeparated
  | -- | Also called "SSV"
    SpaceSeparated
  | -- | Also called "TSV"
    TabSeparated
  | -- | `value1|value2|value2`
    PipeSeparated
  | -- | Using multiple GET parameters, e.g. `foo=bar&foo=baz`. This is valid only for parameters in "query" ('NH.Query') or "formData" ('WH.Form')
    MultiParamArray

toHeaderColl :: WH.ToHttpApiData a => CollectionFormat -> (NH.HeaderName, [a]) -> [NH.Header]
toHeaderColl c = _toColl c toHeader

toFormColl :: WH.ToHttpApiData v => CollectionFormat -> (BC.ByteString, [v]) -> WH.Form
toFormColl c xs = WH.toForm $ fmap unpack $ _toColl c toHeader $ pack xs
  where
    pack (k, v) = (CI.mk k, v)
    unpack (k, v) = (BC.unpack (CI.original k), BC.unpack v)

toQueryColl :: WH.ToHttpApiData a => CollectionFormat -> (BC.ByteString, Maybe [a]) -> NH.Query
toQueryColl c = _toCollA c toQuery

_toColl :: P.Traversable f => CollectionFormat -> (f a -> [(b, BC.ByteString)]) -> f [a] -> [(b, BC.ByteString)]
_toColl c encode xs = fmap (fmap P.fromJust) (_toCollA' c fencode BC.singleton (fmap Just xs))
  where
    fencode = fmap (fmap Just) . encode . fmap P.fromJust
    {-# INLINE fencode #-}

_toCollA :: (P.Traversable f, P.Traversable t, P.Alternative t) => CollectionFormat -> (f (t a) -> [(b, t BC.ByteString)]) -> f (t [a]) -> [(b, t BC.ByteString)]
_toCollA c encode = _toCollA' c encode BC.singleton

_toCollA' :: (P.Monoid c, P.Traversable f, P.Traversable t, P.Alternative t) => CollectionFormat -> (f (t a) -> [(b, t c)]) -> (Char -> c) -> f (t [a]) -> [(b, t c)]
_toCollA' c encode one xs = case c of
  CommaSeparated -> go (one ',')
  SpaceSeparated -> go (one ' ')
  TabSeparated -> go (one '\t')
  PipeSeparated -> go (one '|')
  MultiParamArray -> expandList
  where
    go sep =
      [P.foldl1 (\(sk, sv) (_, v) -> (sk, combine sep <$> sv <*> v <|> sv <|> v)) expandList]
    combine sep x y = x <> sep <> y
    expandList = (P.concatMap encode . (P.traverse . P.traverse) P.toList) xs
    {-# INLINE go #-}
    {-# INLINE expandList #-}
    {-# INLINE combine #-}

-- * AuthMethods

-- | Provides a method to apply auth methods to requests
class
  P.Typeable a =>
  AuthMethod a
  where
  applyAuthMethod ::
    ClientConfig ->
    a ->
    Request req contentType res accept ->
    IO (Request req contentType res accept)

-- | An existential wrapper for any AuthMethod
data AnyAuthMethod = forall a. AuthMethod a => AnyAuthMethod a deriving (P.Typeable)

instance AuthMethod AnyAuthMethod where applyAuthMethod config (AnyAuthMethod a) req = applyAuthMethod config a req

-- | indicates exceptions related to AuthMethods
newtype AuthMethodException = AuthMethodException String deriving (P.Show, P.Typeable)

instance E.Exception AuthMethodException

-- | apply all matching AuthMethods in config to request
_applyAuthMethods ::
  Request req contentType res accept ->
  ClientConfig ->
  IO (Request req contentType res accept)
_applyAuthMethods req config@ClientConfig {configAuthMethods = as} =
  foldlM go req as
  where
    go r (AnyAuthMethod a) = applyAuthMethod config a r

-- * Utils

-- | Removes Null fields.  (OpenAPI-Specification 2.0 does not allow Null in JSON)
_omitNulls :: [(A.Key, A.Value)] -> A.Value
_omitNulls = A.object . P.filter notNull
  where
    notNull (_, A.Null) = False
    notNull _ = True

-- | Encodes fields using WH.toQueryParam
_toFormItem :: (WH.ToHttpApiData a, Functor f) => t -> f a -> f (t, [Text])
_toFormItem name x = (name,) . (: []) . WH.toQueryParam <$> x

-- | Collapse (Just "") to Nothing
_emptyToNothing :: Maybe String -> Maybe String
_emptyToNothing (Just "") = Nothing
_emptyToNothing x = x
{-# INLINE _emptyToNothing #-}

-- | Collapse (Just mempty) to Nothing
_memptyToNothing :: (P.Monoid a, P.Eq a) => Maybe a -> Maybe a
_memptyToNothing (Just x) | x P.== P.mempty = Nothing
_memptyToNothing x = x
{-# INLINE _memptyToNothing #-}

-- * DateTime Formatting

newtype DateTime = DateTime {unDateTime :: TI.UTCTime}
  deriving (P.Eq, P.Data, P.Ord, P.Typeable, NF.NFData)

instance A.FromJSON DateTime where
  parseJSON = A.withText "DateTime" (_readDateTime . T.unpack)

instance A.ToJSON DateTime where
  toJSON (DateTime t) = A.toJSON (_showDateTime t)

instance WH.FromHttpApiData DateTime where
  parseUrlPiece = P.maybe (P.Left "parseUrlPiece @DateTime") P.Right . _readDateTime . T.unpack

instance WH.ToHttpApiData DateTime where
  toUrlPiece (DateTime t) = T.pack (_showDateTime t)

instance P.Show DateTime where
  show (DateTime t) = _showDateTime t

instance MimeRender MimeMultipartFormData DateTime where
  mimeRender _ = mimeRenderDefaultMultipartFormData

-- | @_parseISO8601@
_readDateTime :: (MonadFail m, Alternative m) => String -> m DateTime
_readDateTime s =
  DateTime <$> _parseISO8601 s
{-# INLINE _readDateTime #-}

-- | @TI.formatISO8601Millis@
_showDateTime :: (t ~ TI.UTCTime, TI.FormatTime t) => t -> String
_showDateTime =
  TI.formatISO8601Millis
{-# INLINE _showDateTime #-}

-- | parse an ISO8601 date-time string
_parseISO8601 :: (TI.ParseTime t, MonadFail m, Alternative m) => String -> m t
_parseISO8601 t =
  P.asum $
    P.flip (TI.parseTimeM True TI.defaultTimeLocale) t
      <$> ["%FT%T%QZ", "%FT%T%Q%z", "%FT%T%Q%Z"]
{-# INLINE _parseISO8601 #-}

-- * Date Formatting

newtype Date = Date {unDate :: TI.Day}
  deriving (P.Enum, P.Eq, P.Data, P.Ord, P.Ix, NF.NFData)

instance A.FromJSON Date where
  parseJSON = A.withText "Date" (_readDate . T.unpack)

instance A.ToJSON Date where
  toJSON (Date t) = A.toJSON (_showDate t)

instance WH.FromHttpApiData Date where
  parseUrlPiece = P.maybe (P.Left "parseUrlPiece @Date") P.Right . _readDate . T.unpack

instance WH.ToHttpApiData Date where
  toUrlPiece (Date t) = T.pack (_showDate t)

instance P.Show Date where
  show (Date t) = _showDate t

instance MimeRender MimeMultipartFormData Date where
  mimeRender _ = mimeRenderDefaultMultipartFormData

-- | @TI.parseTimeM True TI.defaultTimeLocale "%Y-%m-%d"@
_readDate :: (Alternative m, MonadFail m) => String -> m Date
_readDate s =
  Date
    <$> ( TI.parseTimeM True TI.defaultTimeLocale "%Y-%m-%d" s
            <|> TI.parseTimeM True TI.defaultTimeLocale "%m%d%Y" s
            <|> TI.parseTimeM True TI.defaultTimeLocale "%m00%Y" s
        )
{-# INLINE _readDate #-}

-- | @TI.formatTime TI.defaultTimeLocale "%Y-%m-%d"@
_showDate :: TI.FormatTime t => t -> String
_showDate =
  TI.formatTime TI.defaultTimeLocale "%Y-%m-%d"
{-# INLINE _showDate #-}

-- * Byte/Binary Formatting

-- | base64 encoded characters
newtype ByteArray = ByteArray {unByteArray :: BL.ByteString}
  deriving (P.Eq, P.Data, P.Ord, P.Typeable, NF.NFData)

instance A.FromJSON ByteArray where
  parseJSON = A.withText "ByteArray" _readByteArray

instance A.ToJSON ByteArray where
  toJSON = A.toJSON . _showByteArray

instance WH.FromHttpApiData ByteArray where
  parseUrlPiece = P.maybe (P.Left "parseUrlPiece @ByteArray") P.Right . _readByteArray

instance WH.ToHttpApiData ByteArray where
  toUrlPiece = _showByteArray

instance P.Show ByteArray where
  show = T.unpack . _showByteArray

instance MimeRender MimeMultipartFormData ByteArray where
  mimeRender _ = mimeRenderDefaultMultipartFormData

-- | read base64 encoded characters
_readByteArray :: MonadFail m => Text -> m ByteArray
_readByteArray = P.either P.fail (pure . ByteArray) . BL64.decode . BL.fromStrict . T.encodeUtf8
{-# INLINE _readByteArray #-}

-- | show base64 encoded characters
_showByteArray :: ByteArray -> Text
_showByteArray = T.decodeUtf8 . BL.toStrict . BL64.encode . unByteArray
{-# INLINE _showByteArray #-}

-- | any sequence of octets
newtype Binary = Binary {unBinary :: BL.ByteString}
  deriving (P.Eq, P.Data, P.Ord, P.Typeable, NF.NFData)

instance A.FromJSON Binary where
  parseJSON = A.withText "Binary" _readBinaryBase64

instance A.ToJSON Binary where
  toJSON = A.toJSON . _showBinaryBase64

instance WH.FromHttpApiData Binary where
  parseUrlPiece = P.maybe (P.Left "parseUrlPiece @Binary") P.Right . _readBinaryBase64

instance WH.ToHttpApiData Binary where
  toUrlPiece = _showBinaryBase64

instance P.Show Binary where
  show = T.unpack . _showBinaryBase64

instance MimeRender MimeMultipartFormData Binary where
  mimeRender _ = unBinary

_readBinaryBase64 :: MonadFail m => Text -> m Binary
_readBinaryBase64 = P.either P.fail (pure . Binary) . BL64.decode . BL.fromStrict . T.encodeUtf8
{-# INLINE _readBinaryBase64 #-}

_showBinaryBase64 :: Binary -> Text
_showBinaryBase64 = T.decodeUtf8 . BL.toStrict . BL64.encode . unBinary
{-# INLINE _showBinaryBase64 #-}

-- * Lens Type Aliases

type Lens_' s a = Lens_ s s a a

type Lens_ s t a b = forall (f :: K.Type -> K.Type). Functor f => (a -> f b) -> s -> f t

-- * Text helpers

decodeLazyUtf8 :: BCL.ByteString -> T.Text
decodeLazyUtf8 = T.decodeUtf8 . BCL.toStrict

encodeLazyUtf8 :: T.Text -> BCL.ByteString
encodeLazyUtf8 = BCL.fromStrict . T.encodeUtf8
