{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE RankNTypes #-} 

module Server (run) where

import Utils.Samples() -- This needs to be imported so that instances of ToSample are declared
import Network.Wai.Handler.Warp (setPort, setBeforeMainLoop, defaultSettings, runSettings)
import Servant ((:>), (:<|>) (..), Proxy(..), Context(..), Application, Server, Handler, serveWithContext)
import System.IO (hPutStrLn, stderr,stdout, hSetBuffering, BufferMode(LineBuffering))
import System.Environment (getEnv)
import Servant.Docs (markdown, docs)
import           Servant.Auth.Server            ( CookieSettings(..)
                                                , JWTSettings
                                                , Auth, Cookie, generateKey, defaultJWTSettings, defaultCookieSettings,
                                                xsrfCookieName, xsrfHeaderName
                                                , defaultXsrfCookieSettings, IsSecure(..)
                                                , SameSite(..)
                                                , XsrfCookieSettings(..)
                                                )

import API.Private.Server (PrivateAPI, privateServer)
import API.Public.Server (PublicAPI, publicServer)

import Database.Selda.PostgreSQL
import Utils.Selda (AppliedWithDB)
import Data.Text (pack, unpack)
import qualified Data.Text.IO as TextIO 
import Control.Monad.IO.Class (liftIO)

import Database.Selda.Validation (diffTable, TableDiff(..))
import Database.Selda
import qualified Models.User as User
import qualified Models.Skill as Skill
import qualified Models.UserSkill as UserSkill
import Data.Function ((&))

import qualified Data.Time as Time

-- * api
type API privateAPI' =
    "public" :> PublicAPI
    :<|> "private" :> privateAPI'

type AppAPI = API (Auth '[Cookie] User.User :> PrivateAPI)

-- Servant Docs doesn't like the type with Servant Auth in it
-- for now, I've just created a new type without autherisation for the docs
type DocsAPI = API PrivateAPI

api :: Proxy AppAPI
api = Proxy



diffTables :: PGConnectInfo -> IO [(String, TableDiff, [Text])]
diffTables connectInfo = withPostgreSQL connectInfo diffedTables

    where
        -- createTable' = compileCreateTable pgPPConfig Ignore
        createTable' _ = ["Create table script is not available since compileCreateTable was hidden in selda-0.4.0.0"]
        tables = 
            [ ("users", diffTable User.users, createTable' User.users)
            , ("skills", diffTable Skill.skills, createTable' Skill.skills)
            , ("users_skills", diffTable UserSkill.usersSkills, createTable' UserSkill.usersSkills)
            ]

        diffTable' :: (String, SeldaT PG IO TableDiff, [Text]) -> SeldaT PG IO (String, TableDiff, [Text])
        diffTable' (tableName, tableDiffM, createTableScript) =
            tableDiffM >>= (\diff -> 
                return (tableName, diff, createTableScript)
            )

        diffedTables :: SeldaT PG IO [(String, TableDiff, [Text])]
        diffedTables = sequence $ diffTable' <$> tables

        
tablesAreDifferent :: [(String, TableDiff, [Text])] -> Bool
tablesAreDifferent = all isDiff
    where
        isDiff (_, TableOK, _) = True
        isDiff _ = False


-- * app
run :: IO ()
run = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    pgHost' <- getEnv "PG_HOST"
    pgPort' <- getEnv "PG_PORT"
    pgPassword' <- getEnv "PG_PASSWORD"
    pgUser' <- getEnv "PG_USER"
    pgDB <- getEnv "PG_DATABASE"
    
    let seldaConnectionInfo = PGConnectInfo {
            pgHost=pack pgHost',
            pgPort=read pgPort',
            pgDatabase=pack pgDB,
            pgSchema=Nothing,
            pgUsername=Just $ pack pgUser',
            pgPassword=Just $ pack pgPassword'
        }
        port = 3000
    
    myKey <- generateKey
    now <- Time.getCurrentTime  

    diffs <- diffTables seldaConnectionInfo

    putStrLn "Create Scripts:\n"
    mapM_ (\diff -> case diff of
        (_,_,[createTableScript]) -> putStrLn $ (unpack createTableScript) ++ ";"
        _ -> putStrLn "AHHH SOME WEIRD THING HAPPENED WHEN PRINTING CREATE SCRIPTS"
        ) diffs
    putStrLn "\n"
    if tablesAreDifferent diffs
    then
        let 
            jwtCfg = defaultJWTSettings myKey
            cookieSettings = defaultCookieSettings
                { cookiePath = Just "/"
                , cookieExpires = Just now
                    { Time.utctDay = Time.addDays 3 (Time.utctDay now) }
                , cookieXsrfSetting = Just $ defaultXsrfCookieSettings
                    { xsrfCookieName = "XSRF-TOKEN"
                    , xsrfHeaderName = "X-XSRF-TOKEN"
                    }
                , cookieIsSecure = NotSecure --Secure
                , cookieSameSite = AnySite --SameSiteStrict
                }
            cfg = cookieSettings :. jwtCfg :. EmptyContext
            settings =
                setPort port $
                setBeforeMainLoop (hPutStrLn stderr ("now listening on port " ++ show port)) $
                defaultSettings

            withDB :: AppliedWithDB
            withDB x = liftIO $ withPostgreSQL seldaConnectionInfo x

            app :: IO Application
            app = 
                return
                    $ serveWithContext api cfg
                    $ server withDB cookieSettings jwtCfg

        in
            runSettings settings =<< app
    
    else
        diffs & filterDiffs & printDiffs
        
        where
            filterDiffs = filter 
                (\(_, x, _) -> 
                    case x of
                        TableOK -> False
                        _ -> True
                )
            
            printDiffs :: [(String, TableDiff, [Text])] -> IO ()
            printDiffs = mapM_ $ \(name, diff, creates) -> do
                putStrLn $ "\n" ++ name ++ ":"
                putStrLn $ '_' : (map (\_ -> '_') name) ++ "\n"
                print diff
                (mapM_ TextIO.putStrLn creates)
                putStrLn "\n\n\n"


    
server :: AppliedWithDB -> CookieSettings -> JWTSettings -> Server AppAPI
server withDB cookieSettings jwtSettings =
    publicServer withDB cookieSettings jwtSettings
    :<|> privateServer withDB docsServer


-- Docs

docsAPI :: Proxy DocsAPI
docsAPI = Proxy

docsServer :: Handler String
docsServer = return $ markdown (docs docsAPI)
