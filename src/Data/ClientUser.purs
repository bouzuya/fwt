module Data.ClientUser
  ( ClientUser(ClientUser)
  ) where

import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, fromObject, fromString, stringify, (.?))
import Data.Eq (class Eq, eq)
import Data.Function (($))
import Data.Show (class Show)
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Data.UserId (UserId)

newtype ClientUser =
  ClientUser
  { id :: UserId
  , name :: String
  }

instance decodeJsonClientUser :: DecodeJson ClientUser where
  decodeJson json = do
    o <- decodeJson json
    id <- o .? "id"
    name <- o .? "name"
    pure $ ClientUser { id, name }

instance encodeJsonClientUser :: EncodeJson ClientUser where
  encodeJson (ClientUser { id, name }) = fromObject $ StrMap.fromFoldable
    [ Tuple "id" $ encodeJson id
    , Tuple "name" $ fromString name
    ]

instance eqClientUser :: Eq ClientUser where
  eq (ClientUser { id: a }) (ClientUser { id: b }) = eq a b

instance showClientUser :: Show ClientUser where
  show user = stringify $ encodeJson user
