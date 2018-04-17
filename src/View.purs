module View (View(..)) where

import Data.Argonaut (class EncodeJson, encodeJson, fromArray, fromObject, fromString, jsonNull, stringify)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (maybe)
import Data.Show (class Show)
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Data.UserStatus (UserStatus)

data View
  = BadRequestView
  | ErrorView
  | NotFoundView
  | OKView
  | UsersView (Array UserStatus)

instance encodeJsonView :: EncodeJson View where
  encodeJson BadRequestView = fromObject $ StrMap.fromFoldable
    [ Tuple "status" $ fromString "BadRequest" ]
  encodeJson ErrorView = fromObject $ StrMap.fromFoldable
    [ Tuple "status" $ fromString "ERROR" ]
  encodeJson NotFoundView = fromObject $ StrMap.fromFoldable
    [ Tuple "status" $ fromString "NotFound" ]
  encodeJson OKView = fromObject $ StrMap.fromFoldable
    [ Tuple "status" $ fromString "OK" ]
  encodeJson (UsersView xs) = fromArray $ encodeJson' <$> xs
    where
      encodeJson' ({ user, fwt }) = fromObject $ StrMap.fromFoldable
        [ Tuple "user" $ encodeJson user
        , Tuple "fwt" $ maybe jsonNull encodeJson fwt
        ]

instance showView :: Show View where
  show x = stringify $ encodeJson x
