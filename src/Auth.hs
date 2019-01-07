{-# LANGUAGE OverloadedStrings, DataKinds,
   TypeOperators, DeriveGeneric, DeriveAnyClass #-}

module Auth (AuthAPI, authHandlers) where

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


generateToken :: User -> Handler (Envelope '[ResponseErr] Token)
generateToken user = pureSuccEnvelope $ Token "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6InJlbmFucm9iZXJ0b0B6b2hvLmNvbSIsInBhc3N3b3JkIjoicmVuYW4xMjMifQ.dN1XpaCTMFjleUr62e4_n05bevn3xXABbfB-cITInck"


authHandlers :: Server AuthAPI
authHandlers = generateToken

