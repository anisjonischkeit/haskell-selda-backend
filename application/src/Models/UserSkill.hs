{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels, DuplicateRecordFields, TypeOperators #-}

module Models.UserSkill where


import           Database.Selda
import           Data.Aeson                     ( ToJSON(..)
                                                , FromJSON(..)
                                                )
import           Servant.Docs                   ( ToSample(..)
                                                , singleSample
                                                )

import Models.Skill            
import Models.User             
import Data.UUID (nil)


data UserSkill = UserSkill
    { user_id :: UUID
    , skill_name :: Text
    , proficiency :: Int
    , hours_spent :: Int
    , desired_work :: Bool
    } deriving (Eq, Show, Read, Generic)
instance SqlRow UserSkill
instance ToJSON UserSkill
instance FromJSON UserSkill
instance ToSample UserSkill where
  toSamples _ = singleSample $ UserSkill 
                                  { user_id = nil
                                  , skill_name          = "React"
                                  , proficiency         = 100
                                  , hours_spent         = 60
                                  , desired_work        = True
                                  }



usersSkills :: Table UserSkill
usersSkills = table
  "users_skills"
  [ pKeySelectorGroup :- primary
  , #skill_name :- foreignKey skills #name
  , #user_id :- foreignKey users #id
  ]

 where
  pKeySelectorGroup :: Group UserSkill (UUID :*: Text)
  pKeySelectorGroup = (#user_id :: Selector UserSkill UUID)
    :+ Single (#skill_name :: Selector UserSkill Text)

