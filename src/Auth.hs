{-# LANGUAGE OverloadedStrings, DataKinds,
   TypeOperators, DeriveGeneric, DeriveAnyClass #-}

module Auth where

import Data.Aeson
import GHC.Generics (Generic)
import Servant
import Servant.Checked.Exceptions
import Network.HTTP.Types.Status


type AuthAPI =
  "auth" :> ReqBody '[JSON] User :> Throws ResponseErr :> Post '[JSON] Token


data ResponseErr = Errors

data Token = Token { token :: String }
  deriving (Generic, ToJSON, FromJSON)

data User = User
  { email :: String
  , password :: String
  } deriving (Generic, ToJSON, FromJSON)
