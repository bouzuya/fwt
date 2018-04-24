module Data.UserStatus
  ( UserStatus(UserStatus)
  ) where

import Control.Bind (bind, pure)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, fromObject, jsonNull, (.?))
import Data.FaceWithTime (FaceWithTime)
import Data.Function (($))
import Data.Maybe (Maybe, maybe)
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Data.User (User)

newtype UserStatus = UserStatus
  { fwt :: Maybe FaceWithTime
  , user :: User
  }

instance decodeJsonUserStatus :: DecodeJson UserStatus where
  decodeJson json = do
    o <- decodeJson json
    user <- o .? "user"
    fwt <- o .? "fwt"
    pure $ UserStatus { user, fwt }

instance encodeJsonUserStatus :: EncodeJson UserStatus where
  encodeJson (UserStatus { user, fwt }) = fromObject $ StrMap.fromFoldable
    [ Tuple "user" $ encodeJson user
    , Tuple "fwt" $ maybe jsonNull encodeJson fwt
    ]
