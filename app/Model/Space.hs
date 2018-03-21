{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Model.Space where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Model.ProjectResponse (ProjectResponse)

data Space = Space
  { name           :: Text
  , description    :: Maybe Text
  , owner          :: Text
  , projects       :: [ProjectResponse]
  , defaultGroupId :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)
