module Data.FWTTime
  ( FWTTime
  , fromInstant
  ) where

import Control.Bind (bind, pure)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson)
import Data.Argonaut as Json
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.Formatter.Parser.Interval (parseDateTime)
import Data.Formatter.Parser.Utils (runP)
import Data.Function (($))
import Data.List (fromFoldable)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)

newtype FWTTime = FWTTime Instant

instance decodeJsonFWTTime :: DecodeJson FWTTime where
  decodeJson json = do
    s <- decodeJson json
    d <- runP parseDateTime s
    pure $ FWTTime $ Instant.fromDateTime d

instance encodeJsonFWTTime :: EncodeJson FWTTime where
  encodeJson t = Json.fromString $ show t

instance showFWTTime :: Show FWTTime where
  show t = toIso8601 t

fromInstant :: Instant -> FWTTime
fromInstant = FWTTime

toIso8601 :: FWTTime -> String
toIso8601 (FWTTime i) = format formatter $ Instant.toDateTime i
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
