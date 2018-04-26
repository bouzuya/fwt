module Route
  ( Action(..)
  , route
  ) where

import Data.Function (($))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))

data Action
  = GetIndex
  | GetUsers
  | UpdateUser String

route :: Method -> Array String -> Maybe Action
route GET [] = Just GetIndex
route GET ["users"] = Just GetUsers
route PUT ["users", id] = Just $ UpdateUser id
route _ _ = Nothing
