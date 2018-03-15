{-# LANGUAGE DeriveGeneric #-}
module Model.ProjectResponse where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Model.PublicURI

data ProjectResponse = ProjectResponse
  { name        :: Text
  , spaceName   :: Text
  , groupId     :: Text
  , version     :: Text
  , description :: Text
  , publicURIs  :: [PublicURI]
  } deriving (Show, Eq, Generic)

instance ToJSON ProjectResponse where
instance FromJSON ProjectResponse where
