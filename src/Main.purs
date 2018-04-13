module Main (main) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Functor ((<$>))
import Data.Semigroup ((<>))
import Data.Show (class Show)
import Data.UUID (GENUUID, UUID, genUUID)
import Prelude (Unit, bind, discard, show, ($))

newtype UserId = UserId UUID
derive newtype instance showUserId :: Show UserId

newtype User = User { id :: UserId, name :: String }
instance showUser :: Show User where
  show (User { id, name }) =
    "User { id: \"" <> show id <> "\", name: \"" <> name <> "\" }"

main :: forall e. Eff (console :: CONSOLE, uuid :: GENUUID | e) Unit
main = do
  userId <- UserId <$> genUUID
  let user = User { id: userId, name: "bouzuya" }
  log $ show user
  log "Hello sailor!"
