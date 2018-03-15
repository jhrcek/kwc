{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Data.Proxy (Proxy (Proxy))
import Model.Space
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API ((:>), BasicAuth, BasicAuthData (BasicAuthData), Get, JSON)
import Servant.Client (BaseUrl (BaseUrl), ClientM, Scheme (Http), client, mkClientEnv, runClientM)

main :: IO ()
main = do
    mgr <- newManager defaultManagerSettings
    let auth = BasicAuthData "testadmin" "admin1234;"
        env = mkClientEnv mgr (BaseUrl Http "localhost" 8080 "business-central/rest")
    runClientM (getSpaces auth) env >>= print

type API = BasicAuth "KIE Workbench Realm" () :> "spaces" :> Get '[JSON] [Space]

api :: Proxy API
api = Proxy

getSpaces :: BasicAuthData -> ClientM [Space]
getSpaces = client api
