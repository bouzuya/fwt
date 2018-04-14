module Route (MyRoute, myRoute) where

import Control.Applicative ((*>), (<$), (<*))
import Data.Foldable (oneOf)
import Data.Functor ((<$>))
import Data.Semigroup ((<>))
import Data.Show (class Show)
import Routing.Match (Match, end, lit, root, str)

data MyRoute
  = GetIndex
  | GetUsers
  | UpdateUser String

instance showMyRoute :: Show MyRoute where
  show GetIndex = "GetIndex"
  show GetUsers = "GetUsers"
  show (UpdateUser id) = "UpdateUser " <> id

getIndex :: Match MyRoute
getIndex = GetIndex <$ (lit "" <* end)

getUsers :: Match MyRoute
getUsers = GetUsers <$ (root *> lit "users" <* end)

updateUser :: Match MyRoute
updateUser = UpdateUser <$> (root *> lit "users" *> str <* end)

myRoute :: Match MyRoute
myRoute = oneOf
  [ getIndex
  , getUsers
  , updateUser
  ]
