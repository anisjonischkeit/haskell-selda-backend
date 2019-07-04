{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}


module API.Public.User (UserAPI, userServer) where

import Servant ((:>), (:<|>)(..), Post, JSON, Server, Handler, ReqBody, PostNoContent, NoContent(..), Headers(..), Header, throwError, err401, err400, err500)
import Servant.Auth.Server (SetCookie, CookieSettings, JWTSettings, acceptLogin)
import Web.Cookie (defaultSetCookie, setCookieName, setCookieValue, setCookiePath, setCookieExpires)
import Servant.API.ResponseHeaders (addHeader)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Maybe (listToMaybe)

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import           Data.Aeson                     ( ToJSON(..)
                                                , FromJSON(..)
                                                )
import           Servant.Docs                   ( ToSample(..), singleSample)

import Crypto.Random (MonadRandom(..))
import           Crypto.Error                   ( CryptoFailable(..) )
import Control.Monad.IO.Class (liftIO)
import           Crypto.KDF.Argon2 (hash, Options(..), Variant(..), Version(..))
import GHC.Generics (Generic(..))
import Data.UUID.V4 as UUID4
import Utils.Selda (AppliedWithDB, safeInsertReturning)
import qualified Models.User as User
import qualified Data.Text as Text
import Database.Selda

-- API

type UserAPI = 
    "login" 
        :> ReqBody '[JSON] LoginDetails 
        :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                        , Header "Set-Cookie" SetCookie]
                                        User.User)
    :<|> "signout" :> PostNoContent '[JSON] 
        (Headers '[ Header "Set-Cookie" SetCookie
                    ,  Header "Set-Cookie" SetCookie]
            NoContent)
    :<|> "signup" :> ReqBody '[JSON] SignupDetails :> Post '[JSON] (Maybe User.User)


userServer :: AppliedWithDB
           -> CookieSettings
           -> JWTSettings
           -> Server UserAPI
userServer withDB cookieSettings jwtSettings
    = userSignin withDB cookieSettings jwtSettings
    :<|> userSignout
    :<|> userSignup withDB


-- SIGNUP

data SignupDetails = SignupDetails 
    { signupEmail :: String
    , signupPassword :: String
    , signupFirstName :: String
    , signupLastName :: String 
    } deriving (Eq, Show, Read, Generic)

instance ToJSON SignupDetails
instance FromJSON SignupDetails

instance ToSample SignupDetails where
    toSamples _ = singleSample SignupDetails
        { signupEmail="anis@anis.com"
        , signupPassword="Abcd1234PW"
        , signupFirstName="Anis"
        , signupLastName="Jonischkeit"
        }
    
userSignup :: AppliedWithDB -> SignupDetails -> Handler (Maybe User.User)
userSignup withDB (SignupDetails email' password fn ln) = do 
    salt <- liftIO $ getRandomBytes 50 :: Handler ByteString
    uuid <- liftIO $ UUID4.nextRandom

    let
        iterations' = 20
        memory' = 2^(16 :: Int)
        parallelism' = 1
        outLen = 16^(2 :: Int)

        options = Options
            { iterations  = fromIntegral iterations'
            , memory      = fromIntegral memory'
            , parallelism = fromIntegral parallelism'
            , variant     = Argon2id
            , version     = Version13
            }
    
    case hash options (pack password) salt outLen of
        CryptoPassed hash' -> 
            let 
                userWithPw = User.UserWithPassword 
                                { User.id=uuid 
                                , User.first_name=Text.pack fn 
                                , User.last_name=Text.pack ln 
                                , User.email=Text.pack email'
                                , User.password_hash=hash'
                                , User.password_salt=salt
                                , User.password_out_len=outLen
                                , User.password_iterations=iterations' 
                                , User.password_memory=memory'
                                , User.password_parallelism=parallelism'
                                }

            in
                do 
                    res <- withDB $ safeInsertReturning User.users [userWithPw]
                    
                    case res of
                        Right resList -> return $ User.withoutPassword <$> (listToMaybe resList)
                        Left _ -> throwError err400 -- Nothing -- TODO: return 400

        CryptoFailed _e ->
            throwError err500 -- return Nothing -- TODO: return 500
            

-- LOGIN

data LoginDetails = LoginDetails 
    { loginEmail :: String
    , loginPassword :: String 
    } deriving (Eq, Show, Read, Generic)


instance ToJSON LoginDetails
instance FromJSON LoginDetails

instance ToSample LoginDetails where
    toSamples _ = singleSample LoginDetails
        { loginEmail="anis@anis.com"
        , loginPassword="Abcd1234PW"
        }
            
userSignin :: AppliedWithDB
           -> CookieSettings
           -> JWTSettings
           -> LoginDetails 
           -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                , Header "Set-Cookie" SetCookie]
                User.User)
userSignin withDB cookieSettings jwtSettings (LoginDetails email' password) = do
    mUserWithPw <- listToMaybe <$> 
        (
            withDB $ query $ do
                user <- select User.users
                restrict (user ! #email .== literal (Text.pack email'))
                return user
        )

    mAuthedUser <- return $ do
        user <- mUserWithPw
        options <- Just $ Options 
            { iterations  = fromIntegral $ User.password_iterations user
            , memory      = fromIntegral $ User.password_memory user
            , parallelism = fromIntegral $ User.password_parallelism user
            , variant     = Argon2id
            , version     = Version13
            }

        case hash options (pack password) (User.password_salt user) (User.password_out_len user) of
            CryptoPassed hash' -> 
                if hash' == User.password_hash user
                then Just user
                else Nothing

            CryptoFailed _e ->
                Nothing

    case mAuthedUser of
        Nothing  -> throwError err401
        Just userWithPw -> do
            let user = User.withoutPassword userWithPw
            mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
            case mApplyCookies of
                Nothing           -> throwError err401
                Just applyCookies -> return $ applyCookies user

            

userSignout :: Handler (Headers '[ Header "Set-Cookie" SetCookie
                , Header "Set-Cookie" SetCookie]
                NoContent)
userSignout =
    let 
        expiredCookie = defaultSetCookie 
            { setCookieValue = ""
            , setCookiePath = Just "/"
            , setCookieExpires = Just $ UTCTime (fromGregorian 1970 1 0) (secondsToDiffTime 0) 
            }
        
        jwtSetCookie = expiredCookie { setCookieName = "JWT-Cookie" }
        xsrfSetCookie = expiredCookie { setCookieName = "XSRF-TOKEN" }

        jwtHeader = addHeader jwtSetCookie NoContent :: Headers '[Header "Set-Cookie" SetCookie] NoContent;
        header = addHeader xsrfSetCookie jwtHeader :: Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent;
    in return header