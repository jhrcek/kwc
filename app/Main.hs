{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API ((:<|>) ((:<|>)), (:>), BasicAuth, BasicAuthData (BasicAuthData), Capture, Delete, Get, JSON, Post, ReqBody)
import Servant.Client (BaseUrl (BaseUrl), ClientEnv, ClientM, Scheme (Http), client, mkClientEnv, runClientM)

import Model.CreateProjectReq as CreateProject
import Model.JobResponse as Job
import Model.JobResult as JobResult
import Model.JobStatus
import Model.ProjectResponse as Project
import Model.Space as Space

main :: IO ()
main = do
    env <- initClientEnv baseUrl
    let KieApiClient{..} = mkKieApiClient authData
        testClient description action  = do
            resp <- runClientM action env
            putStrLn ("----- " ++ description ++ " -----") >> print resp
    testClient "Create space" . waitForJob $ createSpace testSpace
    testClient "Get spaces" getSpaces
    testClient "Get space" . getSpace $ Space.name testSpace
    testClient "Create project" . waitForJob $ createProject (Space.name testSpace) createProjectReq
    testClient "Get projects" . getProjects $ Space.name testSpace
    testClient "Get project" $ getProject (Space.name testSpace) (CreateProject.name createProjectReq)
    testClient "Delete project" . waitForJob $ deleteProject (Space.name testSpace) (CreateProject.name createProjectReq)
    testClient "Delete space" . waitForJob . deleteSpace $ Space.name testSpace

initClientEnv :: BaseUrl -> IO ClientEnv
initClientEnv bu =
    flip mkClientEnv bu <$> newManager defaultManagerSettings

type WorkbenchAPI =
    BasicAuth "KIE Workbench Realm" () :>
          (  -- Spaces
             "spaces" :> Get '[JSON] [Space]
        :<|> "spaces" :> ReqBody '[JSON] Space    :> Post '[JSON] JobResponse
        :<|> "spaces" :> Capture "spaceName" Text :> Get '[JSON] Space
        :<|> "spaces" :> Capture "spaceName" Text :> Delete '[JSON] JobResponse
             -- Projects
        :<|> "spaces" :> Capture "spaceName" Text :> "projects" :> Get '[JSON] [ProjectResponse]
        :<|> "spaces" :> Capture "spaceName" Text :> "projects" :> ReqBody '[JSON] CreateProjectReq :> Post '[JSON] JobResponse
        :<|> "spaces" :> Capture "spaceName" Text :> "projects" :> Capture "projectName" Text :> Get '[JSON] ProjectResponse
        :<|> "spaces" :> Capture "spaceName" Text :> "projects" :> Capture "projectName" Text :> Delete '[JSON] JobResponse
             -- Maven
        :<|> "spaces" :> Capture "spaceName" Text :> "projects" :> Capture "projectName" Text :> "maven" :> "compile" :> Post '[JSON] JobResponse
        :<|> "spaces" :> Capture "spaceName" Text :> "projects" :> Capture "projectName" Text :> "maven" :> "test"    :> Post '[JSON] JobResponse
        :<|> "spaces" :> Capture "spaceName" Text :> "projects" :> Capture "projectName" Text :> "maven" :> "install" :> Post '[JSON] JobResponse
        :<|> "spaces" :> Capture "spaceName" Text :> "projects" :> Capture "projectName" Text :> "maven" :> "deploy"  :> Post '[JSON] JobResponse
             -- Jobs
        :<|> "jobs"   :> Capture "jobId" Text     :> Get '[JSON] JobResult
        :<|> "jobs"   :> Capture "jobId" Text     :> Delete '[JSON] JobResult
          )

workbenchAPI :: Proxy WorkbenchAPI
workbenchAPI = Proxy

data KieApiClient = KieApiClient
    { getSpaces      :: ClientM [Space]
    , createSpace    :: Space -> ClientM JobResponse
    , getSpace       :: Text -> ClientM Space
    , deleteSpace    :: Text -> ClientM JobResponse

    , getProjects    :: Text -> ClientM [ProjectResponse]
    , createProject  :: Text -> CreateProjectReq -> ClientM JobResponse
    , getProject     :: Text -> Text -> ClientM ProjectResponse
    , deleteProject  :: Text -> Text -> ClientM JobResponse -- TODO replace with newtypes SpaceName, ProjectName, JobId

    , compileProject :: Text -> Text -> ClientM JobResponse
    , testProject    :: Text -> Text -> ClientM JobResponse
    , installProject :: Text -> Text -> ClientM JobResponse
    , deployProject  :: Text -> Text -> ClientM JobResponse

    , getJob         :: Text -> ClientM JobResult
    , deleteJob      :: Text -> ClientM JobResult

    , waitForJob     :: ClientM JobResponse -> ClientM JobResult
    }

mkKieApiClient :: BasicAuthData -> KieApiClient
mkKieApiClient basicAuthData =
  let getSpaces
        :<|> createSpace
        :<|> getSpace
        :<|> deleteSpace
        :<|> getProjects
        :<|> createProject
        :<|> getProject
        :<|> deleteProject
        :<|> compileProject
        :<|> testProject
        :<|> installProject
        :<|> deployProject
        :<|> getJob
        :<|> deleteJob
        = client workbenchAPI basicAuthData

      waitForJob  :: ClientM JobResponse -> ClientM JobResult
      waitForJob job =
          job >>= waitForFinish . Job.jobId
        where
          waitForFinish :: Text -> ClientM JobResult
          waitForFinish jid = do
              result <- getJob jid
              let st = JobResult.status result
              if st == SUCCESS || st == FAIL || st == BAD_REQUEST
                then return result
                else do
                    liftIO $ threadDelay 500000 {- 0.5 s -}
                    waitForFinish jid

  in KieApiClient{..}

-- test data
baseUrl :: BaseUrl
baseUrl = BaseUrl Http "localhost" 8080 "business-central/rest"

authData :: BasicAuthData
authData = BasicAuthData "testadmin" "admin1234;"

testSpace :: Space
testSpace = Space "spaceName" (Just "description") "Jan Hrček" [] "cz.janhrcek"

createProjectReq :: CreateProjectReq
createProjectReq = CreateProjectReq "jansProject" "this is cool project" "cz.jan" "1.0.0-SNAPSHOT"
