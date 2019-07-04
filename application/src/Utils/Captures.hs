{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Utils.Captures where

-- type CaptureUserId

import           Data.UUID                      ( UUID )
import Servant.Docs (ToCapture(..), DocCapture(..))
import Servant (Capture)

type UserIdCapture = (Capture "userId" UUID)
instance ToCapture UserIdCapture where
    toCapture _ =
        DocCapture "userId"                          -- name
                   "(UUID) the user's identifier" -- description
        
type SkillNameCapture = (Capture "skillName" String)
instance ToCapture SkillNameCapture where
    toCapture _ =
        DocCapture "skillName"                          -- name
                   "(String) the skill's identifier" -- description
                