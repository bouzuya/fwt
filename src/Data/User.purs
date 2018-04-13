module Data.User (User, user) where

import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.UserId (UserId)

newtype User = User { id :: UserId, name :: String }
instance showUser :: Show User where
  show (User { id, name }) =
    "User { id: \"" <> show id <> "\", name: \"" <> name <> "\" }"

user :: { id :: UserId, name :: String } -> User
user = User
