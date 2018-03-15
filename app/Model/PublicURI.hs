{-# LANGUAGE DeriveGeneric #-}
module Model.PublicURI where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data PublicURI = PublicURI
    { protocol :: Text
    , uri      :: String
    } deriving (Eq, Show, Generic)

instance ToJSON PublicURI where
instance FromJSON PublicURI where
