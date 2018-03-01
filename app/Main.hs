{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens ((&), (?~), (^.))
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.Text (Text)
import Network.Wreq

main :: IO ()
main = do
    r <- getWith opts "https://httpbin.org/basic-auth/user/passwd" >>= asJSON :: IO (Response User)
    print $ r ^. responseBody

opts :: Options
opts = defaults & auth ?~ basicAuth "user" "passwd"

data User = User
  { authenticated :: Bool
  , user          :: Text
  } deriving Show

instance FromJSON User where
    parseJSON = withObject "User" $ \v -> User
        <$> v .: "authenticated"
        <*> v .: "user"
