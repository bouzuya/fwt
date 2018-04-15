module Main (main) where

import Control.Applicative (pure)
import Control.IxMonad ((:*>), (:>>=))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef)
import Control.Monad.State (get, modify, put, runState)
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
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware, lift')
import Hyper.Node.Server (HttpRequest, HttpResponse, defaultOptionsWithLogging, runServer)
import Hyper.Request (RequestData, getRequestData)
import Hyper.Response (ResponseEnded, StatusLineOpen, closeHeaders, respond, writeStatus)
import Hyper.Status (Status, statusNotFound, statusOK)
import Node.HTTP (HTTP)
import Prelude (Unit, bind, discard)
import Route (Action(..), route)

newUser :: String -> UUID -> User
newUser name uuid = user { id: userId uuid, name }

type State = { users :: Array { user :: User, fwt :: Maybe FaceWithTime } }
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
  :: forall e. Ref State
  -> Maybe Action
  -> Eff ( ref :: REF | e ) (Tuple Status String)
view _ (Just GetIndex) = pure $ Tuple statusOK "{\"status\":\"OK\"}" -- TODO: HTML
view ref (Just GetUsers) = do
  { users } <- readRef ref
  pure $ Tuple statusOK $ stringify $ encodeJson $ UsersView users
view _ (Just (UpdateUser _)) = pure $ Tuple statusOK "{\"status\":\"OK\"}"
view _ _ = pure $ Tuple statusNotFound "{\"status\":\"Error\"}"

action :: RequestData -> Maybe Action
action request = do
  method <- either Just (const Nothing) request.method
  path <- Just request.url
  route method path

app
  :: forall e. Ref State
  -> Middleware
      (Aff ( http ∷ HTTP, ref :: REF | e ) )
      (Conn HttpRequest (HttpResponse StatusLineOpen) {})
      (Conn HttpRequest (HttpResponse ResponseEnded) {})
      Unit
app ref =
  getRequestData
    :>>= \request -> (lift' $ liftEff $ view ref $ action request)
    :>>= \(Tuple status body) ->
      writeStatus status :*> closeHeaders :*> respond body

main :: forall e. Eff ( console :: CONSOLE
                      , http :: HTTP
                      , now :: NOW
                      , ref :: REF
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
  ref <- newRef { users }
  runServer defaultOptionsWithLogging {} $ app ref
