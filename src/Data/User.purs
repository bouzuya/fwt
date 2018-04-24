module Data.User (User(User)) where

import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, fromObject, fromString, stringify, (.?))
import Data.Function (($))
import Data.Show (class Show)
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Data.UserId (UserId)

newtype User =
  User
  { id :: UserId
  , name :: String
  , password :: String
  }

instance decodeJsonUser :: DecodeJson User where
  decodeJson json = do
    o <- decodeJson json
    id <- o .? "id"
    name <- o .? "name"
    password <- o .? "password"
    pure $ User { id, name, password }

instance encodeJsonUser :: EncodeJson User where
  encodeJson (User { id, name, password }) = fromObject $ StrMap.fromFoldable
    [ Tuple "id" $ encodeJson id
    , Tuple "name" $ fromString name
    , Tuple "password" $ fromString password
    ]

instance showUser :: Show User where
  show user = stringify $ encodeJson user
