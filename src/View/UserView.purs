module Data.UserView
  ( UserView(UserView)
  ) where

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Argonaut as Json
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Data.User (User(..))
import Prelude (($))

newtype UserView = UserView User

instance encodeJsonUserView :: EncodeJson UserView where
  encodeJson (UserView (User { id, name })) =
    Json.fromObject $ StrMap.fromFoldable
      [ Tuple "id" $ encodeJson id
      , Tuple "name" $ Json.fromString name
      ]
