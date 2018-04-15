module Main (main) where

import Control.Applicative (pure)
import Control.IxMonad (ibind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (NOW, now)
import Data.Argonaut (class EncodeJson, encodeJson, fromArray, fromObject, jsonNull, stringify)
import Data.Either (either)
import Data.FaceWithTime (FaceWithTime, fwt)
import Data.Function (const, ($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..), maybe)
import Data.Show (show)
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Data.URL (url)
import Data.UUID (GENUUID, UUID, genUUID)
import Data.User (User, user)
import Data.UserId (userId)
import Hyper.Node.Server (defaultOptionsWithLogging, runServer)
import Hyper.Request (RequestData, getRequestData)
import Hyper.Response (closeHeaders, respond, writeStatus)
import Hyper.Status (Status, statusNotFound, statusOK)
import Node.HTTP (HTTP)
import Prelude (Unit, bind, discard)
import Route (Action(..), route)

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
  :: Array { user :: User, fwt :: Maybe FaceWithTime }
  -> Maybe Action
  -> Tuple Status String
view _ (Just GetIndex) = Tuple statusOK "{\"status\":\"OK\"}" -- TODO: HTML
view users (Just GetUsers) = Tuple statusOK $ stringify $ encodeJson $ UsersView users
view _ (Just (UpdateUser _)) = Tuple statusOK "{\"status\":\"OK\"}"
view _ _ = Tuple statusNotFound "{\"status\":\"Error\"}"

action :: RequestData -> Maybe Action
action request = do
  method <- either Just (const Nothing) request.method
  path <- Just request.url
  route method path

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
  -- TODO: extract app handler
  let users = [{ user: user', fwt: fwt' }]
  let app = do
        request <- getRequestData
        let (Tuple s b) = view users $ action request
        _ <- writeStatus s
        _ <- closeHeaders
        respond b
        where
          bind = ibind
  runServer defaultOptionsWithLogging {} app
