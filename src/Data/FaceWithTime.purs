module Data.FaceWithTime (FaceWithTime(..), toIso8601) where

import Control.Bind (bind, pure, (<$>), (>>=))
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, fromObject, fromString, stringify, (.?))
import Data.DateTime.Instant (Instant, fromDateTime, toDateTime)
import Data.Either (Either)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.Formatter.Parser.Interval (parseDateTime)
import Data.Formatter.Parser.Utils (runP)
import Data.Function (($))
import Data.List (fromFoldable)
import Data.Semigroup ((<>))
import Data.Show (class Show)
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Data.URL (URL)

newtype FaceWithTime = FaceWithTime
  { face :: URL
  , secret :: String
  , time :: Instant }

instance decodeJsonFaceWithTime :: DecodeJson FaceWithTime where
  decodeJson json = do
    o <- decodeJson json
    face <- o .? "face"
    secret <- o .? "secret"
    time <- o .? "time" >>= fromString'
    pure $ FaceWithTime { face, secret, time }

instance encodeJsonFaceWithTime :: EncodeJson FaceWithTime where
  encodeJson (FaceWithTime { face, secret, time }) =
    fromObject $ StrMap.fromFoldable
      [ Tuple "face" $ encodeJson face
      , Tuple "secret" $ fromString secret
      , Tuple "time" $ fromString $ toIso8601 time
      ]

instance showFaceWithTime :: Show FaceWithTime where
  show fwt = stringify $ encodeJson fwt

fromString' :: String -> Either String Instant
fromString' s = fromDateTime <$> runP parseDateTime s

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
