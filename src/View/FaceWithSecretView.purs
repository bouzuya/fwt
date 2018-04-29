module View.FaceWithSecretView
  ( FaceWithSecretView(FaceWithSecretView)
  ) where

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Argonaut as Json
import Data.FaceWithTime (FaceWithTime(FaceWithTime))
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Prelude (($))

newtype FaceWithSecretView = FaceWithSecretView FaceWithTime

instance encodeJsonFaceWithSecretView :: EncodeJson FaceWithSecretView where
  encodeJson (FaceWithSecretView (FaceWithTime { secret, time })) =
    Json.fromObject $ StrMap.fromFoldable
      [ Tuple "secret" $ Json.fromString secret
      , Tuple "time" $ encodeJson time
      ]
