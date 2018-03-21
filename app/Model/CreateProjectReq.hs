{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Model.CreateProjectReq where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data CreateProjectReq = CreateProjectReq
    { name        :: Text
    , description :: Text
    , groupId     :: Text
    , version     :: Text
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)
