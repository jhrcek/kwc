{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Model.JobResult where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Model.JobStatus

data JobResult = JobResult
    { status         :: JobStatus
    , jobId          :: Text
    , result         :: Maybe Text
    , lastModified   :: Int
    , detailedResult :: Maybe [Text]
    } deriving (Eq, Show, Generic, FromJSON, ToJSON)
