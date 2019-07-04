{-# OPTIONS_GHC -fno-warn-orphans #-}

module Utils.Samples where

import Servant.Docs (ToSample(..), samples, singleSample)
import Web.Cookie (SetCookie, defaultSetCookie)

instance ToSample SetCookie where
    toSamples _ = singleSample defaultSetCookie

instance ToSample Char where
    toSamples _ = samples "abcdefghijklmnopqrstuvwxyz"