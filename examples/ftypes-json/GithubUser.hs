{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module GithubUser where

import JSONFormat
import FTypes

import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as A
import qualified Data.Text as T

newtype GithubUserId
  = GithubUserId
  { unGithubUserId :: Integer
  } deriving (A.FromJSON, A.ToJSON)

newtype Url
  = Url
  { unUrl :: T.Text
  } deriving (A.ToJSON, A.FromJSON)

-- https://api.github.com/users/timjb
data GithubUser
  = GithubUser
  { userLogin :: T.Text
  , userId :: GithubUserId
  , userAvatarUrl :: Url
  -- ...
  }

instance A.ToJSON GithubUser where
  toJSON ghUser =
    A.object
      [ "login" .= userLogin ghUser
      , "id" .= userId ghUser
      , "avatar_url" .= userAvatarUrl ghUser
      ]

instance A.FromJSON GithubUser where
  parseJSON =
    A.withObject "GithubUser" $ \obj -> do
      ghLogin <- obj .: "login"
      ghId <- obj .: "id"
      ghAvatarUrl <- obj .: "avatar_url"
      pure GithubUser
        { userLogin = ghLogin
        , userId = ghId
        , userAvatarUrl = ghAvatarUrl
        }

{-

-- Alternative:

instance A.FromJSON GithubUser where
  parseJSON =
    A.withObject "GithubUser" $ \obj -> do
      GithubUser
        <$> (obj .: "login")
        <*> (obj .: "id")
        <*> (obj .: "avatar_url")

-}

makeFType ''GithubUser

ghUserJSONFormat :: JSONFormat GithubUser
ghUserJSONFormat =
  objectFormat "GithubUser" $
  FGithubUser
    { fuserLogin = field "login"
    , fuserId = field "id"
    , fuserAvatarUrl = field "avatar_url"
    }
