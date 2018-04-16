module Data.User (User(User)) where

import Data.Argonaut (class EncodeJson, encodeJson, fromObject, fromString)
import Data.Function (($))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Data.UserId (UserId)

newtype User = User { id :: UserId, name :: String }

instance encodeJson :: EncodeJson User where
  encodeJson (User { id, name }) = fromObject $ StrMap.fromFoldable
    [ Tuple "id" $ encodeJson id
    , Tuple "name" $ fromString name
    ]

instance showUser :: Show User where
  show (User { id, name }) =
    "User { id: \"" <> show id <> "\", name: \"" <> name <> "\" }"
