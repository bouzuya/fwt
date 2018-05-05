module Data.User
  ( User(User)
  , toClient
  ) where

import Data.ClientUser (ClientUser(..))
import Data.Eq (class Eq, eq)
import Data.UserId (UserId)

newtype User =
  User
  { id :: UserId
  , name :: String
  , password :: String
  }

instance eqUser :: Eq User where
  eq (User { id: a }) (User { id: b }) = eq a b

toClient :: User -> ClientUser
toClient (User { id, name }) =
  ClientUser { id, name }
