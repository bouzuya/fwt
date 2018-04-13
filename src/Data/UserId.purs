module Data.UserId (UserId, userId) where

import Data.Show (class Show)
import Data.UUID (UUID)

newtype UserId = UserId UUID
derive newtype instance showUserId :: Show UserId

userId :: UUID -> UserId
userId = UserId
