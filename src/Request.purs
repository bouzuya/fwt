module Request
  ( createFace
  , getFaces
  , getUsers
  ) where

import Data.Semigroup ((<>))

import Control.Bind (bind, pure)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Argonaut (decodeJson)
import Data.ClientFaceWithTime (ClientFaceWithTime)
import Data.ClientUser (ClientUser)
import Data.Either (either)
import Data.Function (const, ($))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap)
import Data.Tuple (Tuple(..))
import Data.URL (URL(..), parseUrlWithQuery)
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as AX

createFace
  :: forall e
  . { password :: String, userId :: String }
  -> String
  -> Aff (ajax :: AJAX | e) (Maybe (StrMap String))
createFace { password, userId } face = runMaybeT do
  let
    params =
      [ Tuple "password" $ Just password
      , Tuple "user_id" $ Just userId
      ]
  (URL url) <- MaybeT $ pure $ parseUrlWithQuery "/faces" params
  response <- liftAff $ AX.post url ("{\"face\":\"" <> face <> "\"}")
  MaybeT $ pure $ either (const Nothing) Just (decodeJson response.response)

getFaces
  :: forall e
  . { password :: String, secret :: Maybe String, userId :: String }
  -> Aff (ajax :: AJAX | e) (Maybe (Array ClientFaceWithTime))
getFaces { password, secret, userId } = runMaybeT do
  let
    params =
      [ Tuple "password" $ Just password
      , Tuple "secret" $ secret
      , Tuple "user_id" $ Just userId
      ]
  (URL url) <- MaybeT $ pure $ parseUrlWithQuery "/faces" params
  response <- liftAff $ AX.get url
  MaybeT $ pure $ either (const Nothing) Just $ decodeJson response.response

getUsers
  :: forall e
  . { password :: String, userId :: String }
  -> Aff (ajax :: AJAX | e) (Maybe (Array ClientUser))
getUsers { password, userId } = runMaybeT do
  let
    params =
      [ Tuple "password" $ Just password
      , Tuple "user_id" $ Just userId
      ]
  (URL url) <- MaybeT $ pure $ parseUrlWithQuery "/users" params
  response <- liftAff $ AX.get url
  MaybeT $ pure $ either (const Nothing) Just $ decodeJson response.response
