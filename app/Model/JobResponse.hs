{-# LANGUAGE DeriveGeneric #-}

module Model.JobResponse where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Model.JobStatus

data JobResponse = JobResponse
    { jobId  :: Text
    , status :: JobStatus
    } deriving (Eq, Show, Generic)

instance FromJSON JobResponse where
instance ToJSON JobResponse where
