{-# LANGUAGE DeriveGeneric #-}

module Model.JobStatus where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data JobStatus
    = GONE
    | ACCEPTED
    | APPROVED
    | DENIED
    | BAD_REQUEST
    | RESOURCE_NOT_EXIST
    | DUPLICATE_RESOURCE
    | SERVER_ERROR
    | SUCCESS
    | FAIL
    deriving (Eq, Show, Generic)

instance FromJSON JobStatus where
instance ToJSON JobStatus where
