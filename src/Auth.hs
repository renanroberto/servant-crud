{-# LANGUAGE OverloadedStrings, DataKinds,
   TypeOperators, DeriveGeneric, DeriveAnyClass #-}

module Auth (AuthAPI, authHandlers) where

import Data.List
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import qualified Data.ByteString.Base64.Lazy as Base64
import Data.Aeson
import GHC.Generics (Generic)
import Servant
import Servant.Checked.Exceptions
import Network.HTTP.Types.Status


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
generateToken user =
  let
    header = (unpack . Base64.encode . encode) (TokenHeader "jwt" "HS256")
    payload = (unpack . Base64.encode . encode) user
  in
    pureSuccEnvelope $ Token (intercalate "." [header, payload])


authHandlers :: Server AuthAPI
authHandlers = generateToken

