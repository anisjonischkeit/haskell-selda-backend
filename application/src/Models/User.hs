{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels, DuplicateRecordFields #-}

module Models.User
  ( User(..)
  , users
  , withoutPassword
  , UserWithPassword(..)
  )
where

import           Servant.Docs                   ( ToSample(..)
                                                , singleSample
                                                )
import           Database.Selda
import           Data.Aeson                     ( ToJSON(..)
                                                , FromJSON(..)
                                                )
import           Data.UUID                      ( UUID
                                                , nil
                                                )
import           Data.ByteString                ( ByteString )
import           Servant.Auth.Server            ( ToJWT
                                                , FromJWT
                                                )
-- import Models.UserSkill        

data UserWithPassword = UserWithPassword
  { id :: UUID
  , first_name :: Text
  , last_name :: Text
  , email :: Text
  , password_hash :: ByteString
  , password_salt :: ByteString
  , password_out_len :: Int
  , password_iterations :: Int
  , password_memory :: Int
  , password_parallelism :: Int
  } deriving Generic
instance SqlRow UserWithPassword


withoutPassword :: UserWithPassword -> User
withoutPassword (UserWithPassword id' first_name' last_name' email' _ _ _ _ _ _) =
  User id' first_name' last_name' email' []

data User = User
    { id :: UUID
    , first_name :: Text
    , last_name :: Text
    , email :: Text
    , user_skills :: [Text]
    } deriving (Eq, Show, Read, Generic)
instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User
instance ToSample User where
  toSamples _ = singleSample $ User { Models.User.id = nil
                                    , first_name          = "Anis"
                                    , last_name           = "Jonischkeit"
                                    , email = "anis.jonischkeit@gmail.com"
                                    , user_skills         = []
                                    }



users :: Table UserWithPassword
users = table
  "users"
  [ #id :- primary
  , #email :- unique
  ]
