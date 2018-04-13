module Data.FaceWithTime (FaceWithTime, URL, fwt, url) where

import Data.DateTime.Instant (Instant, toDateTime)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List (fromFoldable)
import Data.Maybe (Maybe(Just))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)

newtype URL = URL String
derive newtype instance showURL :: Show URL

newtype FaceWithTime = FaceWithTime { face :: URL, time :: Instant }
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

url :: String -> Maybe URL
url = Just <$> URL -- TODO

fwt :: { face :: URL, time :: Instant } -> FaceWithTime
fwt = FaceWithTime
