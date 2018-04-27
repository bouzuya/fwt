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
  | GetFaces
  | CreateFace

route :: Method -> Array String -> Maybe Action
route GET [] = Just GetIndex
route GET ["users"] = Just GetUsers
route PUT ["users", id] = Just $ UpdateUser id
route GET ["faces"] = Just GetFaces
route POST ["faces"] = Just CreateFace
route _ _ = Nothing
