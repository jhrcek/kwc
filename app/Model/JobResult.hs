{-# LANGUAGE DeriveGeneric #-}

module Model.JobResult where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Model.JobStatus

data JobResult = JobResult
    { status         :: JobStatus
    , jobId          :: Text
    , result         :: Text
    , lastModified   :: Int
    , detailedResult :: [Text]
    } deriving (Eq, Show, Generic)

instance FromJSON JobResult where
instance ToJSON JobResult where
