{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API ((:<|>) ((:<|>)), (:>), BasicAuth, BasicAuthData (BasicAuthData), Capture, Delete, Get, JSON, Post, ReqBody)
import Servant.Client (BaseUrl (BaseUrl), ClientEnv, ClientM, Scheme (Http), client, mkClientEnv, runClientM)

import Model.JobResponse
import Model.JobResult
import Model.Space

main :: IO ()
main = do
    env <- initClientEnv baseUrl
    let KieApiClient{..} = mkKieApiClient authData
        testClient act  = runClientM act env >>= print
    testClient getSpaces
    testClient $ getSpace "myteam"
    Right JobResponse{jobId=jid} <- runClientM (createSpace testSpace) env
    testClient $ getJob jid
    testClient $ deleteJob jid

initClientEnv :: BaseUrl -> IO ClientEnv
initClientEnv bu =
    flip mkClientEnv bu <$> newManager defaultManagerSettings

type WorkbenchAPI =
    BasicAuth "KIE Workbench Realm" () :>
          (  "spaces" :> Get '[JSON] [Space]
        :<|> "spaces" :> Capture "spaceName" Text :> Get '[JSON] Space
        :<|> "spaces" :> ReqBody '[JSON] Space    :> Post '[JSON] JobResponse
        :<|> "jobs"   :> Capture "jobId" Text     :> Get '[JSON] JobResult
        :<|> "jobs"   :> Capture "jobId" Text     :> Delete '[JSON] JobResult
          )

workbenchAPI :: Proxy WorkbenchAPI
workbenchAPI = Proxy

data KieApiClient = KieApiClient
    { getSpaces   :: ClientM [Space]
    , getSpace    :: Text -> ClientM Space
    , createSpace :: Space -> ClientM JobResponse
    , getJob      :: Text -> ClientM JobResult
    , deleteJob   ::  Text -> ClientM JobResult
    }

mkKieApiClient :: BasicAuthData -> KieApiClient
mkKieApiClient basicAuthData =
  let getSpaces :<|> getSpace :<|> createSpace :<|> getJob :<|> deleteJob
        = client workbenchAPI basicAuthData
  in KieApiClient{..}

-- test data
baseUrl :: BaseUrl
baseUrl = BaseUrl Http "localhost" 8080 "business-central/rest"

authData :: BasicAuthData
authData = BasicAuthData "testadmin" "admin1234;"

testSpace :: Space
testSpace = Space "spaceName" (Just "description") "Jan Hrček" [] "cz.janhrcek"
