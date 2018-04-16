module Data.UserStatus (UserStatus) where

import Data.FaceWithTime (FaceWithTime)
import Data.Maybe (Maybe)
import Data.User (User)

type UserStatus = { user :: User, fwt :: Maybe FaceWithTime }
