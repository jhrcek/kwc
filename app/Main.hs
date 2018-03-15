{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API ((:<|>) ((:<|>)), (:>), BasicAuth, BasicAuthData (BasicAuthData), Capture, Get, JSON, Post, ReqBody)
import Servant.Client (BaseUrl (BaseUrl), ClientM, Scheme (Http), client, mkClientEnv, runClientM)

import Model.JobResponse
import Model.Space

main :: IO ()
main = do
    mgr <- newManager defaultManagerSettings
    let env = mkClientEnv mgr (BaseUrl Http "localhost" 8080 "business-central/rest")
        testClient act  = runClientM act env >>= print
    testClient $ getSpaces auth
    testClient $ getSpace auth "myteam"
    testClient $ createSpace auth (Space "spaceName" (Just "description") "Jan Hrček" [] "cz.janhrcek")


auth :: BasicAuthData
auth = BasicAuthData "testadmin" "admin1234;"

type API =
       BasicAuth "KIE Workbench Realm" () :> "spaces" :> Get '[JSON] [Space]
  :<|> BasicAuth "KIE Workbench Realm" () :> "spaces" :> Capture "spaceName" Text :> Get '[JSON] Space
  :<|> BasicAuth "KIE Workbench Realm" () :> "spaces" :> ReqBody '[JSON] Space :> Post '[JSON] JobResponse


api :: Proxy API
api = Proxy


getSpaces :: BasicAuthData -> ClientM [Space]
getSpace  :: BasicAuthData -> Text -> ClientM Space
createSpace :: BasicAuthData -> Space -> ClientM JobResponse
getSpaces :<|> getSpace :<|> createSpace = client api
