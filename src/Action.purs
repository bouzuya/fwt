module Action (doAction) where

import Control.Applicative (pure)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, readRef)
import Data.Argonaut (class DecodeJson, decodeJson, jsonParser, (.?))
import Data.Array (findIndex, modifyAt)
import Data.Either (Either(..))
import Data.FaceWithTime (fwt)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Data.URL (url)
import Data.User (User(User))
import Data.UserStatus (UserStatus)
import Hyper.Status (Status, statusBadRequest, statusNotFound, statusOK)
import Prelude (bind, discard, (==), (>>=))
import Route (Action(..))
import View (View(..))

type State = { users :: Array UserStatus }
newtype UpdateUserBody
  = UpdateUserBody { face :: String }

instance decodeJsonUpdateUserBody :: DecodeJson UpdateUserBody where
  decodeJson json = do
    o <- decodeJson json
    face <- o .? "face"
    pure $ UpdateUserBody { face }

modify' :: forall a. (a -> Boolean) -> (a -> a) -> Array a -> Maybe (Array a)
modify' f g xs = do
  index <- findIndex f xs
  modifyAt index g xs

getUsers :: forall e. Ref State -> Eff ( ref :: REF | e ) (Array UserStatus)
getUsers ref = do
  { users } <- readRef ref
  pure users

updateUser
  :: forall e
  . Ref State
  -> String
  -> UpdateUserBody
  -> Eff ( now :: NOW, ref :: REF | e) (Maybe (Array UserStatus))
updateUser ref id' (UpdateUserBody { face }) = do
  time <- now
  { users } <- readRef ref
  case
    modify'
      (\({ user: (User { id }) }) -> show id == id')
      (\({ user }) ->
        { user
        , fwt: (\f -> fwt { face: f, time }) <$> url face
        })
      users of
    Nothing -> pure Nothing
    (Just newUsers) -> do
      modifyRef ref (\_ -> { users: newUsers })
      pure $ Just newUsers

doAction
  :: forall e. Ref State
  -> Maybe (Tuple Action String)
  -> Eff ( now :: NOW, ref :: REF | e ) (Tuple Status View)
doAction _ (Just (Tuple GetIndex _)) = do
  pure $ Tuple statusOK OKView
doAction ref (Just (Tuple GetUsers _)) = do
  users <- getUsers ref
  pure $ Tuple statusOK (UsersView users)
doAction ref (Just (Tuple (UpdateUser id') body)) = do
  case (jsonParser body >>= decodeJson) :: Either String UpdateUserBody of
    Left _ -> pure $ Tuple statusBadRequest BadRequestView
    Right body' -> do
      result <- updateUser ref id' body'
      pure $ case result of
        Nothing -> Tuple statusNotFound NotFoundView
        (Just _) -> Tuple statusOK OKView
doAction _ _ = do
  pure $ Tuple statusNotFound ErrorView
