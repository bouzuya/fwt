module Data.User (User(User)) where

import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, fromObject, fromString, (.?))
import Data.Function (($))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Data.UserId (UserId)

newtype User = User { id :: UserId, name :: String }

instance decodeJson :: DecodeJson User where
  decodeJson json = do
    o <- decodeJson json
    id <- o .? "id"
    name <- o .? "name"
    pure $ User { id, name }

instance encodeJson :: EncodeJson User where
  encodeJson (User { id, name }) = fromObject $ StrMap.fromFoldable
    [ Tuple "id" $ encodeJson id
    , Tuple "name" $ fromString name
    ]

instance showUser :: Show User where
  show (User { id, name }) =
    "User { id: \"" <> show id <> "\", name: \"" <> name <> "\" }"
