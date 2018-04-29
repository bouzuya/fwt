module View.FaceWithSecretView
  ( FaceWithSecretView(FaceWithSecretView)
  ) where

import Data.Argonaut (class EncodeJson)
import Data.Argonaut as Json
import Data.FaceWithTime (FaceWithTime(FaceWithTime), toIso8601)
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Prelude (($))

newtype FaceWithSecretView = FaceWithSecretView FaceWithTime

instance encodeJsonFaceWithSecretView :: EncodeJson FaceWithSecretView where
  encodeJson (FaceWithSecretView (FaceWithTime { secret, time })) =
    Json.fromObject $ StrMap.fromFoldable
      [ Tuple "secret" $ Json.fromString secret
      , Tuple "time" $ Json.fromString $ toIso8601 time
      ]
