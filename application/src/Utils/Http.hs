{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Utils.Http where

import Servant (ServantErr, throwError, errBody, errHeaders)
import Control.Monad.Except (MonadError)
import qualified Data.ByteString.Lazy as BSL


throwHttpError :: MonadError ServantErr m => ServantErr -> BSL.ByteString -> m a
throwHttpError err msg = 
    throwError $ err {
        -- TODO : Escape the msg so no one can alter your json
        errBody = "{\"message\": \"" <> msg <> "\"}",
        errHeaders = [("Content-type", "application/json")]
    }