module View (View(..)) where

import Data.Argonaut (class EncodeJson, encodeJson, fromArray, fromObject, fromString, jsonNull, stringify)
import Data.FaceWithTime (FaceWithTime)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe, maybe)
import Data.Show (class Show)
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Data.User (User)

data View
  = ErrorView
  | OKView
  | NotFoundView
  | UsersView (Array { user :: User, fwt :: Maybe FaceWithTime })

instance encodeJsonView :: EncodeJson View where
  encodeJson ErrorView = fromObject $ StrMap.fromFoldable
    [ Tuple "status" $ fromString "ERROR" ]
  encodeJson OKView = fromObject $ StrMap.fromFoldable
    [ Tuple "status" $ fromString "OK" ]
  encodeJson NotFoundView = fromObject $ StrMap.fromFoldable
    [ Tuple "status" $ fromString "NotFound" ]
  encodeJson (UsersView xs) = fromArray $ encodeJson' <$> xs
    where
      encodeJson' ({ user, fwt }) = fromObject $ StrMap.fromFoldable
        [ Tuple "user" $ encodeJson user
        , Tuple "fwt" $ maybe jsonNull encodeJson fwt
        ]

instance showView :: Show View where
  show x = stringify $ encodeJson x
