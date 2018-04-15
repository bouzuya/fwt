module Data.UserId (UserId, userId) where

import Data.Argonaut (class EncodeJson, fromString)
import Data.Function (($))
import Data.Show (class Show, show)
import Data.UUID (UUID)

newtype UserId = UserId UUID

instance encodeJsonUserId :: EncodeJson UserId where
  encodeJson (UserId uuid) = fromString $ show uuid

derive newtype instance showUserId :: Show UserId

userId :: UUID -> UserId
userId = UserId
