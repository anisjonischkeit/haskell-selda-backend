{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

module API.Private.User (UserAPI, userServer) where

import Servant ((:>), (:<|>)(..), Get, Post, Put, JSON, Server, Handler, ReqBody, throwError, NoContent(..), err500)
import Control.Monad.IO.Class (liftIO)
import Servant.Docs (ToSample(..)
                                                , singleSample
                                                )

import           Data.UUID                      ( UUID, nil )
import Utils.Http (throwHttpError)
import Data.Aeson (ToJSON(..), FromJSON(..))
import           GHC.Generics                   ( Generic(..) )
import Utils.Selda (AppliedWithDB, unzip, safeInsertReturning)
import Database.Selda
import qualified Models.User as User
import qualified Models.Skill as Skill
import qualified Models.UserSkill as UserSkill
import qualified Data.Text as Text
data AddSkillData = AddSkillData {
    skillName :: String,
    proficiency :: Int,
    hoursSpent :: Int,
    desiredWork :: Bool
} deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)


data UpdateSkillData = UpdateSkillData {
    additions :: [String]
} deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)


instance ToSample AddSkillData where
    toSamples _ = singleSample $ 
        AddSkillData {
            API.Private.User.skillName = "react",
            API.Private.User.proficiency = 100,
            API.Private.User.hoursSpent = 250,
            API.Private.User.desiredWork = True
        }

instance ToSample UpdateSkillData where
    toSamples _ = singleSample $ 
        UpdateSkillData {
            additions = ["react"]
        }

type UserAPI = Get '[JSON] UserData
    :<|> "skills" :> ReqBody '[JSON] AddSkillData :> Post '[JSON] NoContent
    :<|> "skills" :> ReqBody '[JSON] UpdateSkillData :> Put '[JSON] [UserSkill.UserSkill]


userServer :: AppliedWithDB -> UUID -> Server UserAPI
userServer withDB currentUserId = 
    getUserById withDB currentUserId
    :<|> addSkillToUser withDB currentUserId
    :<|> updateUserSkills withDB currentUserId


data UserData = UserData { user :: User.User, skills :: [Skill.Skill] }
    deriving (Eq, Show, Read, Generic, ToJSON)

instance ToSample UserData where
    toSamples _ = singleSample $ 
        UserData {
            user = User.User 
                { User.id = nil
                , User.first_name = "Anis"
                , User.last_name = "Jonischkeit"
                , User.email = "anis.jonischkeit@gmail.com"
                , User.user_skills = []
                },
            skills = [
                Skill.Skill  {
                    Skill.name = "react",
                    Skill.description = "library for doing stuff",
                    Skill.parent_skill = Just "library"
                }
            ]
        }
        

getUserById :: AppliedWithDB -> UUID -> Handler UserData
getUserById withDB userId' = 
    let 
        userQuery = do
            user' <- select User.users
            restrict (user' ! #id .== literal userId')
            return user'

        userSkillsDetailsQuery = do
            userSkill <- select UserSkill.usersSkills
            skill <- select Skill.skills
            restrict (userSkill ! #user_id .== literal userId')
            restrict (userSkill ! #skill_name .== skill ! #name)
            return $ userSkill :*: skill

    in 
        do
            (users :*: skillsTuples) <- 
                withDB $ do 
                    users <- query userQuery
                    userSkills <- query userSkillsDetailsQuery

                    return $ users :*: userSkills

            case users of
                [user'] -> 
                    let (_userSkills :*: skills') = Utils.Selda.unzip skillsTuples

                    in
                        return $ UserData {
                            user = User.withoutPassword user',
                            skills = skills'
                        }
                _ -> throwHttpError err500 "no user found"



addSkillToUser :: AppliedWithDB -> UUID -> AddSkillData -> Handler NoContent
addSkillToUser withDB id' (AddSkillData skillName' proficiency' hoursSpent' desiredWork') = 
    do 
        res <- withDB $ safeInsertReturning 
                            UserSkill.usersSkills 
                            [UserSkill.UserSkill {
                                UserSkill.user_id = id',
                                UserSkill.skill_name = Text.pack skillName',
                                UserSkill.proficiency = proficiency',
                                UserSkill.hours_spent = hoursSpent',
                                UserSkill.desired_work = desiredWork'
                            }]
        case res of
            Right _ -> return NoContent
            Left _ -> throwError err500


updateUserSkills :: AppliedWithDB -> UUID -> UpdateSkillData -> Handler [UserSkill.UserSkill]
updateUserSkills withDB userId' (UpdateSkillData additions') =

    do 
        res <- withDB $ safeInsertReturning 
                            UserSkill.usersSkills 
                            (map (\skillName' -> 
                                    UserSkill.UserSkill {
                                    UserSkill.user_id = userId',
                                    UserSkill.skill_name = Text.pack skillName',
                                    UserSkill.proficiency = 100, -- TODO: Don't hardcode this
                                    UserSkill.hours_spent = 50, -- TODO: Don't hardcode this
                                    UserSkill.desired_work = True -- TODO: Don't hardcode this
                                }
                            ) additions')
        
        case res of
            Right inserted -> return inserted
            Left e -> do
                liftIO $ print e -- TODO: Handle error cases differently
                return [] -- TODO: this should probably be a 400

    --     do
    --         res <- liftIO $ 
    --             (safeRunInsert conn insertSkills)

    --         case res of
    --             Right inserted -> return inserted
    --             Left (ForeignKeyViolation (Right (UsersSkills SkillName))) ->
    --                 throwHttpError err500 "tried to add a skill that doesn't exist" -- TODO make this still add it but as an unsearchable skill
    --             Left e -> do 
    --                 liftIO $ putStrLn $ formatSqlError e
    --                 return []
