{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Model.ProjectResponse where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Model.PublicURI
import Web.HttpApiData (ToHttpApiData, toUrlPiece)

data ProjectResponse = ProjectResponse
  { name        :: ProjectName
  , spaceName   :: Text
  , groupId     :: Text
  , version     :: Text
  , description :: Text
  , publicURIs  :: [PublicURI]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)


newtype ProjectName = ProjectName Text deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance ToHttpApiData ProjectName where
  toUrlPiece (ProjectName pn) = pn
