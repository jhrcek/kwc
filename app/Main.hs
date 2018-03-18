{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API ((:<|>) ((:<|>)), (:>), BasicAuth, BasicAuthData (BasicAuthData), Capture, Delete, Get, JSON, Post, ReqBody)
import Servant.Client (BaseUrl (BaseUrl), ClientEnv, ClientM, Scheme (Http), client, mkClientEnv, runClientM)

import Model.JobResponse as JobResponse
import Model.JobResult as JobResult
import Model.JobStatus
import Model.Space

main :: IO ()
main = do
    env <- initClientEnv baseUrl
    let KieApiClient{..} = mkKieApiClient authData
        testClient act  = runClientM act env >>= print
    testClient . waitForJob $ createSpace testSpace
    testClient getSpaces
    testClient . getSpace $ name testSpace
    testClient . waitForJob . deleteSpace $ name testSpace
    testClient getSpaces

initClientEnv :: BaseUrl -> IO ClientEnv
initClientEnv bu =
    flip mkClientEnv bu <$> newManager defaultManagerSettings

type WorkbenchAPI =
    BasicAuth "KIE Workbench Realm" () :>
          (  "spaces" :> Get '[JSON] [Space]
        :<|> "spaces" :> Capture "spaceName" Text :> Get '[JSON] Space
        :<|> "spaces" :> Capture "spaceName" Text :> Delete '[JSON] JobResponse
        :<|> "spaces" :> ReqBody '[JSON] Space    :> Post '[JSON] JobResponse
        :<|> "jobs"   :> Capture "jobId" Text     :> Get '[JSON] JobResult
        :<|> "jobs"   :> Capture "jobId" Text     :> Delete '[JSON] JobResult
          )

workbenchAPI :: Proxy WorkbenchAPI
workbenchAPI = Proxy

data KieApiClient = KieApiClient
    { getSpaces   :: ClientM [Space]
    , getSpace    :: Text -> ClientM Space
    , deleteSpace :: Text -> ClientM JobResponse
    , createSpace :: Space -> ClientM JobResponse
    , getJob      :: Text -> ClientM JobResult
    , deleteJob   :: Text -> ClientM JobResult
    , waitForJob  :: ClientM JobResponse -> ClientM ()
    }

mkKieApiClient :: BasicAuthData -> KieApiClient
mkKieApiClient basicAuthData =
  let getSpaces :<|> getSpace :<|> deleteSpace :<|> createSpace :<|> getJob :<|> deleteJob
        = client workbenchAPI basicAuthData

      waitForJob  :: ClientM JobResponse -> ClientM ()
      waitForJob job =
          job >>= waitForFinish . JobResponse.jobId
        where
          waitForFinish :: Text -> ClientM ()
          waitForFinish jid = do
              st <- JobResult.status <$> getJob jid
              unless (st == SUCCESS || st == FAIL || st == BAD_REQUEST) $ do
                  liftIO $ putStrLn $ "status was " <> show st
                  liftIO $ threadDelay 1000000
                  waitForFinish jid
  in KieApiClient{..}

-- test data
baseUrl :: BaseUrl
baseUrl = BaseUrl Http "localhost" 8080 "business-central/rest"

authData :: BasicAuthData
authData = BasicAuthData "testadmin" "admin1234;"

testSpace :: Space
testSpace = Space "spaceName" (Just "description") "Jan Hrček" [] "cz.janhrcek"
