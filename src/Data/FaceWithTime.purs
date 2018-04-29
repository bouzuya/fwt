module Data.FaceWithTime
  ( FaceWithTime(..)
  ) where

import Control.Bind (bind, pure)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, fromObject, fromString, stringify, (.?))
import Data.FWTTime (FWTTime)
import Data.Function (($))
import Data.Show (class Show)
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Data.URL (URL)

newtype FaceWithTime = FaceWithTime
  { face :: URL
  , secret :: String
  , time :: FWTTime }

instance decodeJsonFaceWithTime :: DecodeJson FaceWithTime where
  decodeJson json = do
    o <- decodeJson json
    face <- o .? "face"
    secret <- o .? "secret"
    time <- o .? "time"
    pure $ FaceWithTime { face, secret, time }

instance encodeJsonFaceWithTime :: EncodeJson FaceWithTime where
  encodeJson (FaceWithTime { face, secret, time }) =
    fromObject $ StrMap.fromFoldable
      [ Tuple "face" $ encodeJson face
      , Tuple "secret" $ fromString secret
      , Tuple "time" $ encodeJson time
      ]

instance showFaceWithTime :: Show FaceWithTime where
  show fwt = stringify $ encodeJson fwt
