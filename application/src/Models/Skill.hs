{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels #-}

module Models.Skill where

import           Servant.Docs                   ( ToSample(..)
                                                , singleSample
                                                )
import           Database.Selda
import           Data.Aeson                     ( ToJSON(..)
                                                , FromJSON(..)
                                                )

data Skill = Skill
  { name :: Text
  , description  :: Text
  , parent_skill  :: Maybe Text
  } deriving (Eq, Show, Read, Generic)
instance SqlRow Skill
instance ToJSON Skill
instance FromJSON Skill
instance ToSample Skill where
  toSamples _ = singleSample $ Skill { name         = "react"
                                     , description  = "library for doing stuff"
                                     , parent_skill = Just "library"
                                     }

skills :: Table Skill
skills = table
  "skills"
  [ #name :- primary
  , #parent_skill :- foreignKey skills #name
  ]
