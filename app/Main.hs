{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Model.Space
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API ((:<|>) ((:<|>)), (:>), BasicAuth, BasicAuthData (BasicAuthData), Capture, Get, JSON)
import Servant.Client (BaseUrl (BaseUrl), ClientM, Scheme (Http), client, mkClientEnv, runClientM)

main :: IO ()
main = do
    mgr <- newManager defaultManagerSettings
    let auth = BasicAuthData "testadmin" "admin1234;"
        env = mkClientEnv mgr (BaseUrl Http "localhost" 8080 "business-central/rest")
    runClientM (getSpaces auth) env >>= print
    runClientM (getSpace auth "myteam") env >>= print


type API =
       BasicAuth "KIE Workbench Realm" () :> "spaces" :> Get '[JSON] [Space]
  :<|> BasicAuth "KIE Workbench Realm" () :> "spaces" :> Capture "spaceName" Text :> Get '[JSON] Space


api :: Proxy API
api = Proxy


getSpaces :: BasicAuthData -> ClientM [Space]
getSpace  :: BasicAuthData -> Text -> ClientM Space
getSpaces :<|> getSpace = client api
