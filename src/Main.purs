module Main (main) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Functor ((<$>))
import Data.Show (class Show)
import Data.UUID (GENUUID, UUID, genUUID)
import Prelude (Unit, bind, discard, show, ($))

newtype UserId = UserId UUID
derive newtype instance showUserId :: Show UserId

main :: forall e. Eff (console :: CONSOLE, uuid :: GENUUID | e) Unit
main = do
  userId <- UserId <$> genUUID
  log $ show userId
  log "Hello sailor!"
