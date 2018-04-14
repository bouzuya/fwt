module Route
  ( MyRoute(..)
  , myRoute
  ) where

import Control.Applicative ((*>), (<$), (<*))
import Data.Foldable (oneOf)
import Data.Functor ((<$>))
import Data.Semigroup ((<>))
import Data.Show (class Show)
import Routing.Match (Match, end, lit, root, str)

data MyRoute
  = RouteIndex
  | RouteUsers
  | RouteUser String

instance showMyRoute :: Show MyRoute where
  show RouteIndex = "Index"
  show RouteUsers = "Users"
  show (RouteUser id) = "User " <> id

index :: Match MyRoute
index = RouteIndex <$ (lit "" <* end)

users :: Match MyRoute
users = RouteUsers <$ (root *> lit "users" <* end)

user :: Match MyRoute
user = RouteUser <$> (root *> lit "users" *> str <* end)

myRoute :: Match MyRoute
myRoute = oneOf
  [ index
  , users
  , user
  ]
