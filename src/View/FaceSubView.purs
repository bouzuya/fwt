module View.FaceSubView
  ( FaceSubView(FaceSubView)
  ) where

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Argonaut as Json
import Data.FaceWithTime (FaceWithTime(..))
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Prelude (($))

newtype FaceSubView = FaceSubView FaceWithTime

instance encodeJsonFaceSubView :: EncodeJson FaceSubView where
  encodeJson (FaceSubView (FaceWithTime { face, time })) =
    Json.fromObject $ StrMap.fromFoldable
      [ Tuple "face" $ encodeJson face
      , Tuple "time" $ encodeJson time
      ]
