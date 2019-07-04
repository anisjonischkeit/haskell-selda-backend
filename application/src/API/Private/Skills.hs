{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}

module API.Private.Skills (SkillsAPI, skillsServer) where

import Servant ((:>), (:<|>)(..), Get, Post, JSON, Server, Handler, ReqBody, throwError, err500)
import Data.Maybe (listToMaybe)
import Data.Text (pack)

import Database.Selda
import Utils.Selda (AppliedWithDB, WithDB, safeInsertReturning)
import Utils.Captures (SkillNameCapture)

import qualified Models.Skill as Skill

type SkillsAPI = Get '[JSON] [Skill.Skill]
    :<|> SkillNameCapture :> "children" :> Get '[JSON] [Skill.Skill]
    :<|> SkillNameCapture :> Get '[JSON] (Maybe Skill.Skill)
    :<|> ReqBody '[JSON] Skill.Skill :> Post '[JSON] Skill.Skill


skillsServer :: AppliedWithDB -> Server SkillsAPI
skillsServer withDB =
    getSkills withDB
    :<|> getChildSkillsForSkill withDB
    :<|> getSkill withDB
    :<|> addSkill withDB

-- Connection -> SeldaT IO [Skill]
-- getSkills :: Connection -> SeldaT IO a -> Handler [Skill.Skill]
getSkills :: WithDB [Skill.Skill] -> Handler [Skill.Skill]
getSkills withDB = 
    withDB $ query $ select Skill.skills

getChildSkillsForSkill :: (WithDB [Skill.Skill]) -> String -> Handler [Skill.Skill]
getChildSkillsForSkill withDB "null" = 
    withDB $ query $ do
            skill <- select Skill.skills
            restrict (skill ! #parent_skill .== null_)
            return skill

getChildSkillsForSkill withDB skillName = 
    withDB $ query $ do
            skill <- select Skill.skills
            restrict (skill ! #parent_skill .== (just (text $ pack skillName)))
            return skill
        
-- getChildSkillsForSkill con skillName = liftIO $ runQuery con (childSkillsQuery (Just skillName))

getSkill :: (WithDB (Maybe Skill.Skill)) ->  String -> Handler (Maybe Skill.Skill)
getSkill withDB skillName = --liftIO $ listToMaybe <$> runQuery con (skillQuery skillName)
    withDB $ listToMaybe <$> (
        query $ do
            skill <- select Skill.skills
            restrict (skill ! #name .== (text $ pack skillName))
            return skill
    )


addSkill :: AppliedWithDB -> Skill.Skill -> Handler Skill.Skill
addSkill withDB skill = do
    res <- withDB $ safeInsertReturning Skill.skills [skill]
        
    case res of
        Right _ -> return skill
        -- TODO: make error handling better
        Left e -> do 
            liftIO $ do
                putStrLn "we got a problem in here"
                print e
    
            throwError err500



        -- do
        --     res <- liftIO $ safeRunInsert con insertSkill

        --     case res of
        --         Left (UniqueViolation "skills_pkey") -> 
        --             throwHttpError err400 "skill's name must be unique"

        --         Left (ForeignKeyViolation (Right (Skills ParentSkill))) -> 
        --             throwHttpError err400 "the specified parent skill does not exist"

        --         Left _ -> 
        --             throwError err500

        --         Right a -> case listToMaybe a of
        --                         Just x -> return x
        --                         Nothing -> throwError err500
