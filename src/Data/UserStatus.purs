module Data.UserStatus
  ( UserStatus(UserStatus)
  ) where

import Data.FaceWithTime (FaceWithTime)
import Data.Maybe (Maybe)
import Data.User (User)

newtype UserStatus = UserStatus
  { fwt :: Maybe FaceWithTime
  , user :: User
  }
