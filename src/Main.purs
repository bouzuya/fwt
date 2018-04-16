module Main (main) where

import Action (doAction)
import Control.Applicative (pure)
import Control.IxMonad ((:*>), (:>>=))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Ref (REF, Ref, newRef)
import Data.Either (either)
import Data.FaceWithTime (FaceWithTime, fwt)
import Data.Function (const, ($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Data.URL (url)
import Data.UUID (GENUUID, UUID, genUUID)
import Data.User (User(User))
import Data.UserId (userId)
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware, lift')
import Hyper.Node.Server (HttpRequest, HttpResponse, defaultOptionsWithLogging, runServer)
import Hyper.Request (RequestData, getRequestData, readBody)
import Hyper.Response (ResponseEnded, StatusLineOpen, closeHeaders, respond, writeStatus)
import Node.Buffer (BUFFER)
import Node.HTTP (HTTP)
import Prelude (Unit, bind, discard)
import Route (Action, route)

type State = { users :: Array { user :: User, fwt :: Maybe FaceWithTime } }
type RequestBody = String
type ActionWithBody = Tuple Action RequestBody

newUser :: String -> UUID -> User
newUser name uuid = User { id: userId uuid, name }

getRequestData'
  :: forall e res c
  . Middleware
      (Aff ( avar :: AVAR
           , buffer :: BUFFER
           , http ∷ HTTP
           | e
           ))
      (Conn HttpRequest res c)
      (Conn HttpRequest res c)
      (Tuple RequestData RequestBody)
getRequestData' = getRequestData :>>= \request -> Tuple request <$> readBody

actionWithBody :: (Tuple RequestData RequestBody) -> Maybe ActionWithBody
actionWithBody (Tuple request body) = do
  method <- either Just (const Nothing) request.method
  path <- pure $ request.url
  action <- route method path
  pure $ Tuple action body

getActionWithBody
  :: forall e res c
  . Middleware
      (Aff ( avar :: AVAR
           , buffer :: BUFFER
           , http ∷ HTTP
           | e
           ))
      (Conn HttpRequest res c)
      (Conn HttpRequest res c)
      (Maybe ActionWithBody)
getActionWithBody = actionWithBody <$> getRequestData'

app
  :: forall e c. Ref State
  -> Middleware
      (Aff ( avar :: AVAR
           , buffer :: BUFFER
           , http ∷ HTTP
           , now :: NOW
           , ref :: REF
           | e
           ))
      (Conn HttpRequest (HttpResponse StatusLineOpen) c)
      (Conn HttpRequest (HttpResponse ResponseEnded) c)
      Unit
app ref =
  getActionWithBody
  :>>= \action' -> (lift' $ liftEff $ doAction ref action')
  :>>= \(Tuple status view) ->
    writeStatus status
    :*> closeHeaders
    :*> (respond $ show view)

main :: forall e. Eff ( avar :: AVAR
                      , buffer :: BUFFER
                      , console :: CONSOLE
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
