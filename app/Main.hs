{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API ((:<|>) ((:<|>)), (:>), BasicAuth, BasicAuthData (BasicAuthData), Capture, Delete, Get, JSON, Post, ReqBody)
import Servant.Client (BaseUrl (BaseUrl), ClientM, Scheme (Http), client, mkClientEnv, runClientM)

import Model.JobResponse
import Model.JobResult
import Model.Space

main :: IO ()
main = do
    mgr <- newManager defaultManagerSettings
    let env = mkClientEnv mgr (BaseUrl Http "localhost" 8080 "business-central/rest")
        testClient act  = runClientM act env >>= print
    testClient $ getSpaces auth
    testClient $ getSpace auth "myteam"
    Right JobResponse{jobId=jid} <- runClientM (createSpace auth (Space "spaceName" (Just "description") "Jan Hrček" [] "cz.janhrcek")) env
    testClient $ getJob auth jid
    testClient $ deleteJob auth jid


auth :: BasicAuthData
auth = BasicAuthData "testadmin" "admin1234;"

type API =
       BasicAuth "KIE Workbench Realm" () :> "spaces" :> Get '[JSON] [Space]
  :<|> BasicAuth "KIE Workbench Realm" () :> "spaces" :> Capture "spaceName" Text :> Get '[JSON] Space
  :<|> BasicAuth "KIE Workbench Realm" () :> "spaces" :> ReqBody '[JSON] Space :> Post '[JSON] JobResponse
  :<|> BasicAuth "KIE Workbench Realm" () :> "jobs" :> Capture "jobId" Text :> Get '[JSON] JobResult
  :<|> BasicAuth "KIE Workbench Realm" () :> "jobs" :> Capture "jobId" Text :> Delete '[JSON] JobResult

api :: Proxy API
api = Proxy


getSpaces :: BasicAuthData -> ClientM [Space]
getSpace  :: BasicAuthData -> Text -> ClientM Space
createSpace :: BasicAuthData -> Space -> ClientM JobResponse
getJob :: BasicAuthData -> Text -> ClientM JobResult
deleteJob ::  BasicAuthData -> Text -> ClientM JobResult
getSpaces :<|> getSpace :<|> createSpace :<|> getJob :<|> deleteJob = client api
