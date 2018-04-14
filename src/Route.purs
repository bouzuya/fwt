module Route (MyRoute, myRoute) where

import Control.Applicative ((*>), (<$), (<*))
import Data.Foldable (oneOf)
import Data.Functor ((<$>))
import Data.Semigroup ((<>))
import Data.Show (class Show)
import Routing.Match (Match, end, lit, root, str)

data MyRoute
  = Index
  | Users
  | User String

instance showMyRoute :: Show MyRoute where
  show Index = "Index"
  show Users = "Users"
  show (User id) = "User " <> id

index :: Match MyRoute
index = Index <$ (lit "" <* end)

users :: Match MyRoute
users = Users <$ (root *> lit "users" <* end)

user :: Match MyRoute
user = User <$> (root *> lit "users" *> str <* end)

myRoute :: Match MyRoute
myRoute = oneOf
  [ index
  , users
  , user
  ]
