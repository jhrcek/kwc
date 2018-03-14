{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API ((:>), BasicAuth, BasicAuthData (BasicAuthData), Get, JSON)
import Servant.Client (BaseUrl (BaseUrl), ClientM, Scheme (Http), client,
                       mkClientEnv, runClientM)
main :: IO ()
main = do
    mgr <- newManager defaultManagerSettings
    let auth = BasicAuthData "john" "123"
        env = mkClientEnv mgr (BaseUrl Http "httpbin.org" 80 "basic-auth")
    runClientM (basicAuthClient auth) env >>= print

type API = BasicAuth "Fake realm" () :> "john" :> "123" :> Get '[JSON] User

api :: Proxy API
api = Proxy

data User = User
  { authenticated :: Bool
  , user          :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON User where
instance ToJSON User where

basicAuthClient :: BasicAuthData -> ClientM User
basicAuthClient = client api
