{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Model.CloneProjectReq where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data CloneProjectReq = CloneProjectReq
    { name        :: Text
    , description :: Text
    , userName    :: Text
    , password    :: Text
    , gitURL      :: Text
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)
