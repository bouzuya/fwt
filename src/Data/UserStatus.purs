module Data.UserStatus
  ( UserStatus(UserStatus)
  ) where

import Data.Argonaut (class EncodeJson, encodeJson, fromObject, jsonNull)
import Data.FaceWithTime (FaceWithTime)
import Data.Function (($))
import Data.Maybe (Maybe, maybe)
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Data.User (User)

newtype UserStatus = UserStatus { user :: User, fwt :: Maybe FaceWithTime }

instance encodeJsonUserStatus :: EncodeJson UserStatus where
  encodeJson (UserStatus { user, fwt }) = fromObject $ StrMap.fromFoldable
    [ Tuple "user" $ encodeJson user
    , Tuple "fwt" $ maybe jsonNull encodeJson fwt
    ]
