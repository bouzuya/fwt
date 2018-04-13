module Main (main) where

import Control.Applicative (pure)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.DateTime.Instant (instant)
import Data.FaceWithTime (fwt, url)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Show (show)
import Data.Time.Duration (Milliseconds(Milliseconds)) as DTD
import Data.UUID (GENUUID, genUUID)
import Data.User (user)
import Data.UserId (userId)
import Prelude (Unit, bind, discard)

main :: forall e. Eff (console :: CONSOLE, uuid :: GENUUID | e) Unit
main = do
  userId' <- userId <$> genUUID
  let user' = user { id: userId', name: "bouzuya" }
  log $ show user'
  let fwt' = do
        face <- url "https://bouzuya.net/"
        time <- instant $ DTD.Milliseconds 1523616518300.0
        pure $ fwt { face, time }
  log $ show fwt'
  log "Hello sailor!"
