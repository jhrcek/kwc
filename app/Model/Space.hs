{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Model.Space where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Model.ProjectResponse (ProjectResponse)
import Web.HttpApiData (ToHttpApiData, toUrlPiece)

data Space = Space
  { name           :: SpaceName
  , description    :: Maybe Text
  , owner          :: Text
  , projects       :: [ProjectResponse]
  , defaultGroupId :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype SpaceName = SpaceName Text deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance ToHttpApiData SpaceName where
  toUrlPiece (SpaceName sn) = sn
