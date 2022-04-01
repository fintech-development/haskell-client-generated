{-# LANGUAGE OverloadedStrings #-}

module Network.Integrated.HTTP.Auth where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.Data as P (Typeable, typeOf)
import qualified Data.Foldable as P
import Data.Function ((&))
import qualified Data.Text.Encoding as T
import qualified Lens.Micro as L
import Network.Integrated.HTTP.Core
import Prelude (($), (/=))
import qualified Prelude as P

-- * Auth Methods

-- ** AuthBasic

data AuthBasic
  = -- | username password
    AuthBasic B.ByteString B.ByteString
  deriving (P.Eq, P.Show, P.Typeable)

instance AuthMethod AuthBasic where
  applyAuthMethod _ a@(AuthBasic user pw) req =
    P.pure $
      if P.typeOf a `P.elem` rAuthTypes req
        then
          req `setHeader` toHeader ("Authorization", T.decodeUtf8 cred)
            & L.over rAuthTypesL (P.filter (/= P.typeOf a))
        else req
    where
      cred = BC.append "Basic " (B64.encode $ BC.concat [user, ":", pw])

-- ** OAuth20 Token

newtype Auth20BearerToken
  = -- | token
    Auth20BearerToken B.ByteString
  deriving (P.Eq, P.Show, P.Typeable)

instance AuthMethod Auth20BearerToken where
  applyAuthMethod _ a@(Auth20BearerToken token) req =
    P.pure $
      if P.typeOf a `P.elem` rAuthTypes req
        then
          req `setHeader` toHeader ("Authorization", T.decodeUtf8 cred)
            & L.over rAuthTypesL (P.filter (/= P.typeOf a))
        else req
    where
      cred = BC.append "Bearer " token
