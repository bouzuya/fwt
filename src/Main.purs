module Main (main) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.DateTime.Instant (Instant, instant, toDateTime)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.Functor ((<$>))
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..), maybe)
import Data.Semigroup ((<>))
import Data.Show (class Show)
import Data.Time.Duration (Milliseconds(Milliseconds)) as DTD
import Data.UUID (GENUUID, UUID, genUUID)
import Prelude (Unit, bind, discard, show, ($))

newtype UserId = UserId UUID
derive newtype instance showUserId :: Show UserId

newtype User = User { id :: UserId, name :: String }
instance showUser :: Show User where
  show (User { id, name }) =
    "User { id: \"" <> show id <> "\", name: \"" <> name <> "\" }"

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

main :: forall e. Eff (console :: CONSOLE, uuid :: GENUUID | e) Unit
main = do
  userId <- UserId <$> genUUID
  let user = User { id: userId, name: "bouzuya" }
  log $ show user
  let face = URL "https://bouzuya.net/"
  let now = instant $ DTD.Milliseconds 1523616518300.0
  let fwt = maybe Nothing (\time -> Just $ FaceWithTime { face, time }) now
  log $ show fwt
  log "Hello sailor!"
