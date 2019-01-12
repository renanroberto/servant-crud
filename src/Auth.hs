{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleInstances,
   TypeOperators, DeriveGeneric, DeriveAnyClass #-}

module Auth (AuthAPI, authHandlers) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Lazy (toStrict, fromStrict)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL.Lazy as Base64

import Crypto.Hash
import Crypto.MAC.HMAC

import Data.List (intercalate)
import Data.Aeson
import GHC.Generics (Generic)

import Servant
import Servant.Checked.Exceptions
import Network.HTTP.Types.Status


{--}
key :: ByteString
key = "senha super secreta"

pass :: HMAC SHA256
pass = hmac key ("renan123" :: ByteString)

crypto :: ByteString -> Bool
crypto str =
  let hs = hmac key str in hs == pass
--}


type AuthAPI =
  "auth" :> ReqBody '[JSON] User :> Throws ResponseErr :> Post '[JSON] String


data ResponseErr = InvalidCredentials

instance ErrStatus ResponseErr where
  toErrStatus InvalidCredentials = status401

instance ToJSON ResponseErr where
  toJSON InvalidCredentials = toJSON ("Email or password are wrong" :: String)


data User = User
  { email :: String
  , password :: String
  } deriving (Generic, ToJSON, FromJSON)

data TokenHeader = TokenHeader
  { typ :: String
  , sig :: String
  } deriving (Generic, ToJSON, FromJSON)


instance Show (HMAC a) where
  show = show . hmacGetDigest


class Joinable a where
  (<.>) :: a -> a -> a

instance Joinable ByteString where
  x <.> y = BS.intercalate "." [x, y]

instance Joinable [Char] where
  x <.> y = intercalate "." [x, y]


encodeToBS :: ToJSON a => a -> ByteString
encodeToBS = (toStrict . Base64.encode . encode)

tokenHeader :: ByteString
tokenHeader =  encodeToBS (TokenHeader "jwt" "hs256")

tokenPayload :: User -> ByteString
tokenPayload user = encodeToBS user

tokenSecret :: ByteString -> HMAC SHA256
tokenSecret msg =
  hmac key msg
    where key :: ByteString
          key = "meu grande segredo"


generateToken :: User -> Handler (Envelope '[ResponseErr] String)
generateToken user = 
    pureSuccEnvelope $ unpack tokenData <.> show (tokenSecret tokenData)
      where tokenData :: ByteString
            tokenData = tokenHeader <.> tokenPayload user


authHandlers :: Server AuthAPI
authHandlers = generateToken

