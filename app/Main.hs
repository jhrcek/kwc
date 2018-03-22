{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API (BasicAuthData (BasicAuthData))
import Servant.Client (BaseUrl (BaseUrl), ClientEnv, Scheme (Http), mkClientEnv, runClientM)

import Model.CloneProjectReq
import Model.CreateProjectReq as CreateProject
import Model.ProjectResponse
import Model.Space as Space
import Workbench.Client

main :: IO ()
main = do
    env <- initClientEnv baseUrl
    let WorkbenchClient{..} = mkWorkbenchClient authData
        testClient description action  = do
            resp <- runClientM action env
            putStrLn ("----- " ++ description ++ " -----") >> print resp
    testClient "Create space" . waitForJob $ createSpace testSpace
    testClient "Get spaces" getSpaces
    testClient "Get space" . getSpace $ Space.name testSpace
    testClient "Create project" . waitForJob $ createProject (Space.name testSpace) createProjectReq
    testClient "Clone project" . waitForJob $ cloneProject (Space.name testSpace) cloneProjectReq
    testClient "Get projects" . getProjects $ Space.name testSpace
    testClient "Get project" $ getProject (Space.name testSpace) (CreateProject.name createProjectReq)
    testClient "Delete project" . waitForJob $ deleteProject (Space.name testSpace) (CreateProject.name createProjectReq)
    testClient "Delete space" . waitForJob . deleteSpace $ Space.name testSpace

initClientEnv :: BaseUrl -> IO ClientEnv
initClientEnv bu =
    flip mkClientEnv bu <$> newManager defaultManagerSettings

-- test data
baseUrl :: BaseUrl
baseUrl = BaseUrl Http "localhost" 8080 "business-central/rest"

authData :: BasicAuthData
authData = BasicAuthData "testadmin" "admin1234;"

testSpace :: Space
testSpace = Space (SpaceName "spaceName") (Just "description") "Jan Hrček" [] "cz.janhrcek"

createProjectReq :: CreateProjectReq
createProjectReq = CreateProjectReq (ProjectName "jansProject") "this is cool project" "cz.jan" "1.0.0-SNAPSHOT"

cloneProjectReq :: CloneProjectReq
cloneProjectReq = CloneProjectReq "My Employee Rostering" "Some description" "" "" "file:///home/hrk/Tmp/Employee-Rostering"
