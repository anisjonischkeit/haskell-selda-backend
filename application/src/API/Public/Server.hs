{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module API.Public.Server
    ( publicServer
    , PublicAPI
    )
where

import           Servant                        ( (:>)
                                                , Server
                                                )
import           Servant.Auth.Server            ( CookieSettings
                                                , JWTSettings
                                                )


import           API.Public.User                ( UserAPI
                                                , userServer
                                                )
import Utils.Selda (AppliedWithDB)

-- * api
type PublicAPI = "user" :> UserAPI

publicServer :: AppliedWithDB -> CookieSettings -> JWTSettings -> Server PublicAPI
publicServer = userServer
