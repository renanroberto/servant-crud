{-# LANGUAGE OverloadedStrings, DataKinds,
   TypeOperators, DeriveGeneric, DeriveAnyClass #-}

module Auth (AuthAPI, authHandlers) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64
import Data.ByteString (ByteString)
import Data.List (intercalate)
import Crypto.Hash
import Crypto.MAC.HMAC
import Data.Aeson
import GHC.Generics (Generic)
import Servant
import Servant.Checked.Exceptions
import Network.HTTP.Types.Status

key :: ByteString
key = "senha super secreta"

pass :: HMAC SHA256
pass = hmac key ("renan123" :: ByteString)

crypto :: ByteString -> Bool
crypto str =
  let hs = hmac key str in hs == pass


type AuthAPI =
  "auth" :> ReqBody '[JSON] User :> Throws ResponseErr :> Post '[JSON] Token


data ResponseErr = InvalidCredentials

instance ErrStatus ResponseErr where
  toErrStatus InvalidCredentials = status401

instance ToJSON ResponseErr where
  toJSON InvalidCredentials = toJSON ("Email or password are wrong" :: String)


data Token = Token { token :: String }
  deriving (Generic, ToJSON, FromJSON)

data User = User
  { email :: String
  , password :: String
  } deriving (Generic, ToJSON, FromJSON)

data TokenHeader = TokenHeader
  { typ :: String
  , sig :: String
  } deriving (Generic, ToJSON, FromJSON)


generateToken :: User -> Handler (Envelope '[ResponseErr] Token)
generateToken user = undefined
  -- let
  --   header = (show . Base64.encode . encode) (TokenHeader "jwt" "HS256")
  --   payload = (show . Base64.encode . encode) user
  -- in
  --   pureSuccEnvelope $ Token (intercalate "." [header, payload])


authHandlers :: Server AuthAPI
authHandlers = generateToken

