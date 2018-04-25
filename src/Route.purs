module Route
  ( Action(..)
  , route
  ) where

import Control.Applicative ((*>), (<$), (<*), (<*>))
import Control.Bind ((>>=))
import Data.Either (either)
import Data.Foldable (oneOf)
import Data.Function (const, ($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..)) as Method
import Data.HTTP.Method (Method)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (class Show)
import Routing (match)
import Routing.Match (Match, end, lit, params, root, str)

data Path
  = PathIndex
  | PathUsers (Map String String)
  | PathUser String

instance showPath :: Show Path where
  show PathIndex = "Index"
  show (PathUsers _) = "Users"
  show (PathUser id) = "User " <> id

data Action
  = GetIndex
  | GetUsers (Map String String)
  | UpdateUser String

pathIndex :: Match Path
pathIndex = PathIndex <$ (lit "" <* end)

pathUsers :: Match Path
pathUsers = PathUsers <$ (root *> lit "users") <*> params <* end

pathUser :: Match Path
pathUser = PathUser <$> (root *> lit "users" *> str <* end)

paths :: Match Path
paths = oneOf
  [ pathIndex
  , pathUsers
  , pathUser
  ]

path :: String -> Maybe Path
path url = either (const Nothing) Just $ match paths url

action :: Method -> Path -> Maybe Action
action Method.GET PathIndex = Just GetIndex
action Method.GET (PathUsers params) = Just $ GetUsers params
action Method.PUT (PathUser id) = Just $ UpdateUser id
action _ _ = Nothing

route :: Method -> String -> Maybe Action
route method url = path url >>= action method
