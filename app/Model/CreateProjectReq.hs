{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Model.CreateProjectReq where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Model.ProjectResponse (ProjectName)

data CreateProjectReq = CreateProjectReq
    { name        :: ProjectName
    , description :: Text
    , groupId     :: Text
    , version     :: Text
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)
