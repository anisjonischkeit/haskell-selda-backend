{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}

module API.Private.Server
    ( privateServer
    , PrivateAPI
    )
where

import           Servant                        ( (:>)
                                                , (:<|>) (..)
                                                , Server
                                                , err401
                                                , err403
                                                , Handler
                                                , Get
                                                , PlainText
                                                )
import           Servant.Auth.Server            ( AuthResult
                                                    ( Authenticated
                                                    , Indefinite
                                                    )
                                                , throwAll
                                                )
import           API.Private.Skills              ( SkillsAPI
                                                , skillsServer
                                                )
import           API.Private.Users              ( UsersAPI
                                                , usersServer
                                                )
import           API.Private.User              ( UserAPI
                                                , userServer
                                                )
import           Models.User                    ( User(..) )
import Utils.Selda

-- * api
type PrivateAPI
    = "skills" :> SkillsAPI
    :<|> "users" :> UsersAPI
    :<|> "user" :> UserAPI
    :<|> "docs" :> Get '[PlainText] String
    
privateServer :: AppliedWithDB -> Handler String -> AuthResult User -> Server PrivateAPI
privateServer withDB docsServer (Authenticated user)
    = skillsServer withDB
    :<|> usersServer withDB
    :<|> userServer withDB ( Models.User.id user)
    :<|> docsServer

    -- where
    --     -- withDB :: forall (m :: * -> *) a. (MonadIO m, MonadThrow m, MonadMask m) => SeldaT m a -> m a
    --     withDB = (withPostgreSQL seldaConn)

privateServer _ _ Indefinite = throwAll err403
privateServer _ _ _ = throwAll err401
