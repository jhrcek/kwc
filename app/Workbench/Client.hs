{-# LANGUAGE RecordWildCards #-}

module Workbench.Client where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Servant.API ((:<|>) ((:<|>)), BasicAuthData)
import Servant.Client (ClientM, client)

import Model.CloneProjectReq as CloneProject
import Model.CreateProjectReq as CreateProject
import Model.JobResponse as Job
import Model.JobResult as JobResult
import Model.JobStatus
import Model.ProjectResponse as Project
import Model.Space as Space
import Workbench.API

data WorkbenchClient = WorkbenchClient
    { -- Spaces
      getSpaces      :: ClientM [Space]
    , createSpace    :: Space -> ClientM JobResponse
    , getSpace       :: SpaceName -> ClientM Space
    , deleteSpace    :: SpaceName -> ClientM JobResponse
    -- Projects
    , getProjects    :: SpaceName -> ClientM [ProjectResponse]
    , createProject  :: SpaceName -> CreateProjectReq -> ClientM JobResponse
    , getProject     :: SpaceName -> ProjectName -> ClientM ProjectResponse
    , deleteProject  :: SpaceName -> ProjectName -> ClientM JobResponse
    -- Git
    , cloneProject   :: SpaceName -> CloneProjectReq -> ClientM JobResponse
    -- Maven
    , compileProject :: SpaceName -> ProjectName -> ClientM JobResponse
    , testProject    :: SpaceName -> ProjectName -> ClientM JobResponse
    , installProject :: SpaceName -> ProjectName -> ClientM JobResponse
    , deployProject  :: SpaceName -> ProjectName -> ClientM JobResponse
    -- Jobs
    , getJob         :: Text -> ClientM JobResult
    , deleteJob      :: Text -> ClientM JobResult
    , waitForJob     :: ClientM JobResponse -> ClientM JobResult
    }

mkWorkbenchClient :: BasicAuthData -> WorkbenchClient
mkWorkbenchClient basicAuthData =
  let getSpaces
        :<|> createSpace
        :<|> getSpace
        :<|> deleteSpace
        :<|> getProjects
        :<|> createProject
        :<|> getProject
        :<|> deleteProject
        :<|> cloneProject
        :<|> compileProject
        :<|> testProject
        :<|> installProject
        :<|> deployProject
        :<|> getJob
        :<|> deleteJob
        = client (Proxy :: Proxy WorkbenchAPI) basicAuthData

      waitForJob  :: ClientM JobResponse -> ClientM JobResult
      waitForJob job =
          job >>= waitForFinish . Job.jobId
        where
          waitForFinish :: Text -> ClientM JobResult
          waitForFinish jid = do
              result <- getJob jid
              let status = JobResult.status result
              if status `elem` [SUCCESS, FAIL, BAD_REQUEST, SERVER_ERROR]
                then return result
                else do
                    liftIO $ threadDelay 500000 {- 0.5 s -}
                    waitForFinish jid

  in WorkbenchClient{..}
