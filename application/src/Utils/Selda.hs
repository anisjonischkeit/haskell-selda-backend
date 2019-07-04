{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Utils.Selda where

import           Database.Selda (SeldaM, (:*:)(..), SeldaError, SeldaM, Table, insert, Relational)
import           Database.Selda.PostgreSQL (PG)
import           Servant                        ( Handler )
import Control.Monad.Catch (catch)


type WithDB a = SeldaM PG a -> Handler a

-- Since this function gets passed around, we can never have something
-- like 
--     `f :: Something -> (SeldaT IO a -> IO a) -> ReturnType` since that
-- would mean we could use (and see) `a` somewhere else (like in `ReturnType a`).
-- This is why we need to write it like this:
--     `f :: Something -> (forall a. SeldaT IO a -> IO a) -> ReturnType`
type AppliedWithDB = forall a . WithDB a

-- copy pasted from http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.List.html#unzip
-- and applied to :*:
unzip :: [(a :*: b)] -> ([a] :*: [b])
unzip = foldr (\(a :*: b) ~(as :*: bs) -> (a:as :*: b:bs)) ([] :*: [])

safeInsertReturning :: (Relational a) => Table a -> [a] -> SeldaM PG (Either SeldaError [a])
safeInsertReturning tbl data' =
    catch insertReturning eHandler

    where 
        eHandler :: SeldaError -> SeldaM PG (Either SeldaError a)
        eHandler e = return $ Left e

        insertDataReturning :: (Relational a) => Table a -> [a] -> SeldaM PG [a]
        insertDataReturning tbl' data'' = do
            _ <- insert tbl' data''
            return data''

        -- insertReturning :: SeldaM PG (Either SeldaError [a])
        insertReturning = (fmap Right returnedData)
            where
                returnedData = insertDataReturning tbl data'
