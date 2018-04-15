module Data.FaceWithTime (FaceWithTime, fwt) where

import Data.Argonaut (class EncodeJson, encodeJson, fromObject, fromString)
import Data.DateTime.Instant (Instant, toDateTime)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.Function (($))
import Data.List (fromFoldable)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Data.URL (URL)

newtype FaceWithTime = FaceWithTime { face :: URL, time :: Instant }

instance encodeJsonFaceWithTime :: EncodeJson FaceWithTime where
  encodeJson (FaceWithTime { face, time }) =
    fromObject $ StrMap.fromFoldable
      [ Tuple "face" $ encodeJson face
      , Tuple "time" $ fromString $ toIso8601 time
      ]

instance showFaceWithTime :: Show FaceWithTime where
  show (FaceWithTime { face, time }) =
    "FaceWithTime" <>
    " { face: \"" <> show face <> "\"" <>
    " , time: \"" <> toIso8601 time <> "\"" <>
    " }"

toIso8601 :: Instant -> String
toIso8601 i = format formatter $ toDateTime i
  where
    dateFormat =
      [ YearFull
      , Placeholder "-"
      , MonthTwoDigits
      , Placeholder "-"
      , DayOfMonthTwoDigits
      ]
    dateTimeFormat = dateFormat <> [Placeholder "T"] <> timeFormat
    timeFormat =
      [ Hours24
      , Placeholder ":"
      , MinutesTwoDigits
      , Placeholder ":"
      , SecondsTwoDigits
      ]
    formatter = fromFoldable $ dateTimeFormat <> [Placeholder "Z"]

fwt :: { face :: URL, time :: Instant } -> FaceWithTime
fwt = FaceWithTime
