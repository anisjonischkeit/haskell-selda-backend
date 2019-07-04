{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module API.Public.Server
    ( publicServer
    , PublicAPI
    )
where

import           Servant                        ( (:>)
                                                , (:<|>) (..)
                                                , Server
                                                , Handler
                                                , Get
                                                , PlainText
                                                )
import           Servant.Auth.Server            ( CookieSettings
                                                , JWTSettings
                                                )


import           API.Public.User                ( UserAPI
                                                , userServer
                                                )
import Utils.Selda (AppliedWithDB)

-- * api
type PublicAPI 
    = "user" :> UserAPI
    :<|> "docs" :> Get '[PlainText] String

publicServer :: AppliedWithDB -> Handler String -> CookieSettings -> JWTSettings -> Server PublicAPI
publicServer withDB docsServer cookieSettings jwtSettings 
    = userServer withDB cookieSettings jwtSettings
    :<|> docsServer
