{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Model.PublicURI where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data PublicURI = PublicURI
    { protocol :: Text
    , uri      :: String
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)
