module View.UserStatusView
  ( UserStatusView(UserStatusView)
  ) where

import Data.Argonaut (class EncodeJson, encodeJson, jsonNull)
import Data.Argonaut as Json
import Data.Maybe (maybe)
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Data.UserStatus (UserStatus(..))
import Prelude (($))

newtype UserStatusView = UserStatusView UserStatus

instance encodeJsonUserStatusView :: EncodeJson UserStatusView where
  encodeJson (UserStatusView (UserStatus { user, fwt })) =
    Json.fromObject $ StrMap.fromFoldable
      [ Tuple "user" $ encodeJson user
      , Tuple "fwt" $ maybe jsonNull encodeJson fwt
      ]
