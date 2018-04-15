module Main (main) where

import Control.Applicative (pure)
import Control.IxMonad (ibind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (NOW, now)
import Data.Argonaut (class EncodeJson, encodeJson, fromArray, fromObject, jsonNull, stringify)
import Data.Either (Either(..))
import Data.FaceWithTime (FaceWithTime, fwt)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HTTP.Method (CustomMethod, Method)
import Data.HTTP.Method (Method(..)) as Method
import Data.Maybe (Maybe, maybe)
import Data.Show (show)
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Data.URL (url)
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

newtype UsersView =
  UsersView (Array { user :: User, fwt :: Maybe FaceWithTime })

instance encodeJsonUsersView :: EncodeJson UsersView where
  encodeJson (UsersView xs) = fromArray $ encodeJson' <$> xs
    where
      encodeJson' ({ user, fwt }) = fromObject $ StrMap.fromFoldable
        [ Tuple "user" $ encodeJson user
        , Tuple "fwt" $ maybe jsonNull encodeJson fwt
        ]

view
  :: Either Method CustomMethod
  -> MyRoute
  -> Array { user :: User, fwt :: Maybe FaceWithTime }
  -> String
view (Left Method.GET) RouteIndex _ = "OK" -- TODO: HTML
view (Left Method.GET) RouteUsers users = stringify $ encodeJson $ UsersView users
view (Left Method.PATCH) (RouteUser _) _ = "{\"status\":\"OK\"}"
view _ _ _ = "ERROR"

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
  let users = [{ user: user', fwt: fwt' }]
  let app = do
        request <- getRequestData
        _ <- writeStatus statusOK
        _ <- closeHeaders
        case match myRoute request.url of
          (Left _) -> respond "ERROR"
          (Right route) -> respond $ view request.method route users
        where
          bind = ibind
  runServer defaultOptionsWithLogging {} app
