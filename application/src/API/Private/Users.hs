{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLabels #-}

module API.Private.Users (UsersAPI, usersServer) where

import Servant ((:>), (:<|>)(..), Get, JSON, Server, Handler)
import Data.Maybe (listToMaybe)

import           Data.UUID                      ( UUID )
import Utils.Captures (UserIdCapture)
import Utils.Selda (AppliedWithDB)
import qualified Models.User as User
import Database.Selda

type UsersAPI = Get '[JSON] [User.User]
    -- :<|> ReqBody '[JSON] UserWithPassword :> Post '[JSON] (Maybe User)
    :<|> UserIdCapture :> Get '[JSON] (Maybe User.User)


usersServer :: AppliedWithDB -> Server UsersAPI
usersServer withDB = getUsers withDB
    -- :<|> addUser conn
    :<|> getUserById withDB

getUsers :: AppliedWithDB -> Handler [User.User]
getUsers withDB = do 
    mUser <- withDB $ query $ select User.users
    return $ User.withoutPassword <$> mUser

getUserById :: AppliedWithDB -> UUID -> Handler (Maybe User.User)
getUserById withDB id' = do 
    mUser <- listToMaybe <$> 
        (
            withDB $ query $ do
                user <- select User.users
                restrict (user ! #id .== literal id')
                return user
        )

    return $ User.withoutPassword <$> mUser

-- Should never need this
-- addUser :: Connection -> UserWithPassword -> Handler (Maybe User)
-- addUser con user = liftIO $ do 
--     mUser <- listToMaybe <$>
--         runInsertManyReturning con userTable [userToPG user] id

--     return $ withoutPassword <$> mUser