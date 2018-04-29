module Data.FaceWithTime
  ( FaceWithTime(..)
  , toClient
  ) where

import Control.Bind (bind, pure)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, fromObject, fromString, stringify, (.?))
import Data.ClientFaceWithTime (ClientFaceWithTime(..))
import Data.FWTTime (FWTTime)
import Data.Function (($))
import Data.Show (class Show)
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Data.URL (URL)
import Data.UserId (UserId)

newtype FaceWithTime = FaceWithTime
  { face :: URL
  , secret :: String
  , time :: FWTTime
  , userId :: UserId
  }

instance decodeJsonFaceWithTime :: DecodeJson FaceWithTime where
  decodeJson json = do
    o <- decodeJson json
    face <- o .? "face"
    secret <- o .? "secret"
    time <- o .? "time"
    userId <- o .? "user_id"
    pure $ FaceWithTime { face, secret, time, userId }

instance encodeJsonFaceWithTime :: EncodeJson FaceWithTime where
  encodeJson (FaceWithTime { face, secret, time, userId }) =
    fromObject $ StrMap.fromFoldable
      [ Tuple "face" $ encodeJson face
      , Tuple "secret" $ fromString secret
      , Tuple "time" $ encodeJson time
      , Tuple "user_id" $ encodeJson userId
      ]

instance showFaceWithTime :: Show FaceWithTime where
  show fwt = stringify $ encodeJson fwt

toClient :: FaceWithTime -> ClientFaceWithTime
toClient (FaceWithTime { face, time, userId }) =
  ClientFaceWithTime { face, time, userId }
