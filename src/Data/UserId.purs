module Data.UserId (UserId, userId) where

import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, fromString)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Maybe (maybe)
import Data.Show (class Show, show)
import Data.UUID (UUID, parseUUID)

newtype UserId = UserId UUID

instance decodeJsonUserId :: DecodeJson UserId where
  decodeJson json = do
    o <- decodeJson json
    uuid <- maybe (Left "invalid uuid") pure $ parseUUID o
    pure $ UserId uuid

instance encodeJsonUserId :: EncodeJson UserId where
  encodeJson (UserId uuid) = fromString $ show uuid

derive newtype instance showUserId :: Show UserId

userId :: UUID -> UserId
userId = UserId
