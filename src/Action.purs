module Action (doAction) where

import Control.Applicative (pure)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, readRef)
import Data.Argonaut (class DecodeJson, decodeJson, jsonParser, (.?))
import Data.Array (findIndex, modifyAt)
import Data.Either (Either(..))
import Data.FaceWithTime (fwt)
import Data.Foldable (find)
import Data.Function (flip, ($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..), maybe)
import Data.Show (show)
import Data.Tuple (Tuple(..), fst, snd)
import Data.URL (url)
import Data.User (User(User))
import Data.UserId (UserId(..))
import Data.UserStatus (UserStatus(..))
import Hyper.Status (Status, statusBadRequest, statusForbidden, statusNotFound, statusOK)
import Prelude (bind, discard, (&&), (==), (>>=))
import Route (Action(..))
import View (View(..))

type State = { users :: Array UserStatus }
type Query = Array (Tuple String (Maybe String))
type Body = String
type ActionWithParams = { action :: Action, body :: Body, query :: Query }
type Credentials = { password :: String, userId :: String }
newtype UpdateUserBody
  = UpdateUserBody { face :: String }

instance decodeJsonUpdateUserBody :: DecodeJson UpdateUserBody where
  decodeJson json = do
    o <- decodeJson json
    face <- o .? "face"
    pure $ UpdateUserBody { face }

credentials :: Query -> Maybe Credentials
credentials query = do
  let lookup k a = maybe Nothing snd $ find (\t -> fst t == k) a
  password <- lookup "password" query
  userId <- lookup "user_id" query
  pure { password, userId }

authenticate :: Query -> Array UserStatus -> Maybe UserStatus
authenticate query users =
  credentials query >>= (flip authenticatedUser users)

authenticatedUser
  :: Credentials
  -> Array UserStatus
  -> Maybe UserStatus
authenticatedUser { password, userId } users =
  find
    (\(UserStatus
      { user: (User { id: (UserId uuid), password: p })
      }) ->
        show uuid == userId && p == password
    )
    users

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
      (\(UserStatus { user: (User { id }) }) -> show id == id')
      (\(UserStatus { user }) ->
          UserStatus
            { user
            , fwt: (\f -> fwt { face: f, time }) <$> url face
            })
      users of
    Nothing -> pure Nothing
    (Just newUsers) -> do
      modifyRef ref (\_ -> { users: newUsers })
      pure $ Just newUsers

handleGetIndexAction :: forall e. Eff e (Tuple Status View)
handleGetIndexAction = pure $ Tuple statusOK IndexView

handleGetUsersAction
  :: forall e. Ref State
  -> Query
  -> Eff ( ref :: REF | e ) (Tuple Status View)
handleGetUsersAction ref query = do
  users <- getUsers ref
  case authenticate query users of
    Nothing -> pure $ Tuple statusForbidden ForbiddenView
    (Just _) -> pure $ Tuple statusOK (UsersView users)

handleUpdateUserAction
  :: forall e. Ref State
  -> String
  -> Body
  -> Eff ( now :: NOW, ref :: REF | e ) (Tuple Status View)
handleUpdateUserAction ref id' body = do
  case (jsonParser body >>= decodeJson) :: Either String UpdateUserBody of
    Left _ -> pure $ Tuple statusBadRequest BadRequestView
    Right body' -> do
      result <- updateUser ref id' body'
      pure $ case result of
        Nothing -> Tuple statusNotFound NotFoundView
        (Just _) -> Tuple statusOK OKView

handleGetFacesAction
  :: forall e. Ref State
  -> Query
  -> Eff ( ref :: REF | e ) (Tuple Status View)
handleGetFacesAction _ _ = pure $ Tuple statusNotFound ErrorView

handleCreateFaceAction
  :: forall e. Ref State
  -> Body
  -> Eff ( ref :: REF | e ) (Tuple Status View)
handleCreateFaceAction _ _ = pure $ Tuple statusNotFound ErrorView

handleDefaultAction :: forall e. Eff e (Tuple Status View)
handleDefaultAction = pure $ Tuple statusNotFound ErrorView

doAction
  :: forall e. Ref State
  -> Maybe ActionWithParams
  -> Eff ( now :: NOW, ref :: REF | e ) (Tuple Status View)
doAction _ (Just { action: GetIndex }) =
  handleGetIndexAction
doAction ref (Just { action: GetUsers, query }) =
  handleGetUsersAction ref query
doAction ref (Just { action: (UpdateUser id'), body }) =
  handleUpdateUserAction ref id' body
doAction ref (Just { action: GetFaces, query }) =
  handleGetFacesAction ref query
doAction ref (Just { action: CreateFace, body }) =
  handleCreateFaceAction ref body
doAction _ _ =
  handleDefaultAction
