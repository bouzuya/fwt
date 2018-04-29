module Data.ClientFaceWithTime
  ( ClientFaceWithTime(..)
  ) where

import Control.Bind (bind, pure)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, (.?))
import Data.Argonaut as Json
import Data.FWTTime (FWTTime)
import Data.Function (($))
import Data.Show (class Show)
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Data.URL (URL)
import Data.UserId (UserId)

newtype ClientFaceWithTime =
  ClientFaceWithTime
    { face :: URL
    , time :: FWTTime
    , userId :: UserId
    }

instance decodeJsonClientFaceWithTime :: DecodeJson ClientFaceWithTime where
  decodeJson json = do
    o <- decodeJson json
    face <- o .? "face"
    time <- o .? "time"
    userId <- o .? "user_id"
    pure $ ClientFaceWithTime { face, time, userId }

instance encodeJsonClientFaceWithTime :: EncodeJson ClientFaceWithTime where
  encodeJson (ClientFaceWithTime { face, time, userId }) =
    Json.fromObject $ StrMap.fromFoldable
      [ Tuple "face" $ encodeJson face
      , Tuple "time" $ encodeJson time
      , Tuple "user_id" $ encodeJson userId
      ]

instance showFaceWithTime :: Show ClientFaceWithTime where
  show fwt = Json.stringify $ encodeJson fwt
