{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Workbench.API where

import Data.Text (Text)
import Servant.API ((:<|>), (:>), BasicAuth, Capture, Delete, Get, JSON, Post, ReqBody)

import Model.CloneProjectReq (CloneProjectReq)
import Model.CreateProjectReq (CreateProjectReq)
import Model.JobResponse (JobResponse)
import Model.JobResult (JobResult)
import Model.ProjectResponse (ProjectName, ProjectResponse)
import Model.Space (Space, SpaceName)


type WorkbenchAPI =
    BasicAuth "KIE Workbench Realm" () :>
             -- Spaces
          (  "spaces" :> Get '[JSON] [Space]
        :<|> "spaces" :> ReqBody '[JSON] Space         :> Post '[JSON] JobResponse
        :<|> "spaces" :> Capture "spaceName" SpaceName :> Get '[JSON] Space
        :<|> "spaces" :> Capture "spaceName" SpaceName :> Delete '[JSON] JobResponse
             -- Projects
        :<|> "spaces" :> Capture "spaceName" SpaceName :> "projects" :> Get '[JSON] [ProjectResponse]
        :<|> "spaces" :> Capture "spaceName" SpaceName :> "projects" :> ReqBody '[JSON] CreateProjectReq  :> Post '[JSON] JobResponse
        :<|> "spaces" :> Capture "spaceName" SpaceName :> "projects" :> Capture "projectName" ProjectName :> Get '[JSON] ProjectResponse
        :<|> "spaces" :> Capture "spaceName" SpaceName :> "projects" :> Capture "projectName" ProjectName :> Delete '[JSON] JobResponse
              -- Git
        :<|> "spaces" :> Capture "spaceName" SpaceName :> "git" :> "clone" :> ReqBody '[JSON] CloneProjectReq :> Post '[JSON] JobResponse
             -- Maven
        :<|> "spaces" :> Capture "spaceName" SpaceName :> "projects" :> Capture "projectName" ProjectName :> "maven" :> "compile" :> Post '[JSON] JobResponse
        :<|> "spaces" :> Capture "spaceName" SpaceName :> "projects" :> Capture "projectName" ProjectName :> "maven" :> "test"    :> Post '[JSON] JobResponse
        :<|> "spaces" :> Capture "spaceName" SpaceName :> "projects" :> Capture "projectName" ProjectName :> "maven" :> "install" :> Post '[JSON] JobResponse
        :<|> "spaces" :> Capture "spaceName" SpaceName :> "projects" :> Capture "projectName" ProjectName :> "maven" :> "deploy"  :> Post '[JSON] JobResponse
             -- Jobs
        :<|> "jobs" :> Capture "jobId" Text :> Get '[JSON] JobResult
        :<|> "jobs" :> Capture "jobId" Text :> Delete '[JSON] JobResult
          )
