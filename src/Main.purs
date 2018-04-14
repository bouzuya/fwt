module Main (main) where

import Control.Applicative (pure)
import Control.IxMonad (ibind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (NOW, now)
import Data.Either (Either(..))
import Data.FaceWithTime (fwt, url)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Show (show)
import Data.UUID (GENUUID, UUID, genUUID)
import Data.User (User, user)
import Data.UserId (userId)
import Hyper.Node.Server (defaultOptionsWithLogging, runServer)
import Hyper.Request (getRequestData)
import Hyper.Response (closeHeaders, respond, writeStatus)
import Hyper.Status (statusOK)
import Node.HTTP (HTTP)
import Prelude (Unit, bind, discard)
import Route (MyRoute(..), myRoute)
import Routing (match)

newUser :: String -> UUID -> User
newUser name uuid = user { id: userId uuid, name }

main :: forall e. Eff ( console :: CONSOLE
                      , http :: HTTP
                      , now :: NOW
                      , uuid :: GENUUID
                      | e
                      )
                      Unit
main = do
  -- TODO: test data
  user' <- newUser "bouzuya" <$> genUUID
  log $ show $ user'
  instant' <- now
  let fwt' = do
        face <- url "https://bouzuya.net/"
        time <- pure $ instant'
        pure $ fwt { face, time }
  log $ show fwt'
  -- TODO: test route
  log $ show $ match myRoute "/"
  log $ show $ match myRoute "/users"
  log $ show $ match myRoute "/users/abc"
  -- TODO: extract app handler
  let users = [user']
  let app = do
        request <- getRequestData
        _ <- writeStatus statusOK
        _ <- closeHeaders
        case match myRoute request.url of
          (Left _) -> respond "ERROR"
          (Right RouteIndex) -> respond "OK" -- TODO: HTML
          (Right RouteUsers) -> respond $ show users -- TODO: JSON
          (Right (RouteUser id)) -> respond $ show user' -- TODO: JSON
        where
          bind = ibind
  runServer defaultOptionsWithLogging {} app
