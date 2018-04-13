module Main (main) where

import Prelude (Unit, bind, discard, show, ($))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.UUID (GENUUID, genUUID)

main :: forall e. Eff (console :: CONSOLE, uuid :: GENUUID | e) Unit
main = do
  uuid' <- genUUID
  log $ show uuid'
  log "Hello sailor!"
