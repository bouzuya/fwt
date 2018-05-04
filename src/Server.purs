module Server (main) where

import Action (doAction)
import Control.Applicative (pure)
import Control.IxMonad ((:*>), (:>>=))
import Control.Monad.Aff (Aff, error)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, throwException)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF, Ref, newRef)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (decodeJson, jsonParser)
import Data.Either (either)
import Data.Function (const, id, ($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..), maybe)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.UUID (GENUUID, genUUID)
import Data.User (User(User))
import Data.UserId (userId)
import Data.UserStatus (UserStatus(..))
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware, lift')
import Hyper.Middleware.Class (getConn)
import Hyper.Node.FileServer (fileServer)
import Hyper.Node.Server (HttpRequest, HttpResponse, defaultOptionsWithLogging, runServer)
import Hyper.Request (RequestData, getRequestData, parseUrl, readBody)
import Hyper.Response (ResponseEnded, StatusLineOpen, closeHeaders, respond, writeStatus)
import Node.Buffer (BUFFER, fromString)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.HTTP (HTTP)
import Node.Process (PROCESS, cwd, lookupEnv)
import Prelude (Unit, bind, (=<<))
import Route (Action, route)

type State = { users :: Array UserStatus }
type Components = { ref :: Ref State }
type RequestBody = String
type ActionWithBody =
  { action :: Action
  , body ::  RequestBody
  , query :: Array (Tuple String (Maybe String))
  }

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
  let parsedUrl = parseUrl request.url
  path <- pure $ parsedUrl.path
  query <- pure $ either (const []) id $ parsedUrl.query
  action <- route method path
  pure { action, body, query }

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
  :: forall e
  . Middleware
      (Aff ( avar :: AVAR
           , buffer :: BUFFER
           , http ∷ HTTP
           , now :: NOW
           , ref :: REF
           , uuid :: GENUUID
           | e
           ))
      (Conn HttpRequest (HttpResponse StatusLineOpen) Components)
      (Conn HttpRequest (HttpResponse ResponseEnded) Components)
      Unit
app =
  getConn
  :>>= \conn -> Tuple conn.components.ref <$> getActionWithBody
  :>>= \(Tuple ref action') -> (lift' $ liftEff $ doAction ref action')
  :>>= \(Tuple status view) -> (lift' $ liftEff $ Tuple status <$> fromString (show view) UTF8)
  :>>= \(Tuple status' buf) ->
    writeStatus status'
    :*> closeHeaders
    :*> (respond buf)

parseUsers :: String -> Maybe (Array { name :: String, password :: String })
parseUsers json = do
  usersMap <- maps json
  usersRecord <- sequence $ mapToRecord <$> usersMap
  pure usersRecord
  where
    maps :: String -> Maybe (Array (StrMap String))
    maps s = either (const Nothing) Just $ decodeJson =<< jsonParser s
    mapToRecord :: StrMap String -> Maybe { name :: String, password :: String }
    mapToRecord map = do
      name <- StrMap.lookup "name" map
      password <- StrMap.lookup "password" map
      pure { name, password }

recordToUserStatus
  :: forall e
  . { name :: String, password :: String }
  -> Eff (uuid :: GENUUID | e) UserStatus
recordToUserStatus { name, password } = do
  user <- User <$> ({ id: _, name, password }) <$> (userId <$> genUUID)
  pure $ UserStatus { fwt: Nothing, user }

loadUserStatuses
  :: forall e
  . Eff
    ( process :: PROCESS
    , uuid :: GENUUID
    | e
    )
    (Maybe (Array UserStatus))
loadUserStatuses = runMaybeT do
  json <- MaybeT $ lookupEnv "FWT_USERS"
  records <- MaybeT $ pure $ parseUsers json
  lift $ sequence $ recordToUserStatus <$> records

main :: forall e. Eff ( avar :: AVAR
                      , buffer :: BUFFER
                      , console :: CONSOLE
                      , exception :: EXCEPTION
                      , fs :: FS
                      , http :: HTTP
                      , now :: NOW
                      , process :: PROCESS
                      , ref :: REF
                      , uuid :: GENUUID
                      | e
                      )
                      Unit
main = do
  loaded <- loadUserStatuses
  users <- maybe (throwException (error "no users")) pure loaded
  ref <- newRef { users }
  let components = { ref }
  currentDirectory <- cwd
  runServer defaultOptionsWithLogging components $
    fileServer (currentDirectory <> "/public") app
