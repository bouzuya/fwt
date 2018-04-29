module Action (doAction) where

import Control.Applicative (pure)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, readRef)
import Data.Argonaut (class DecodeJson, decodeJson, jsonParser, (.?))
import Data.Array (findIndex, modifyAt)
import Data.Either (Either(..), either)
import Data.FaceWithTime (fwt)
import Data.Foldable (find)
import Data.Function (const, flip, ($))
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

getUserStatuses
  :: forall e. Ref State -> Eff ( ref :: REF | e ) (Array UserStatus)
getUserStatuses ref = do
  { users } <- readRef ref
  pure users

updateUser
  :: forall e
  . Ref State
  -> User
  -> UpdateUserBody
  -> Eff ( now :: NOW, ref :: REF | e) (Maybe (Array UserStatus))
updateUser ref user (UpdateUserBody { face }) = do
  time <- now
  { users } <- readRef ref
  case
    modify'
      (\(UserStatus { user: user' }) -> user == user')
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
  users <- getUserStatuses ref
  pure $ if authenticate query users
    then Tuple statusOK (UsersView users)
    else Tuple statusForbidden ForbiddenView
  where
    authenticate query users = maybe false (const true) $
      credentials query >>= (flip authenticatedUser users)

handleGetFacesAction
  :: forall e. Ref State
  -> Query
  -> Eff ( ref :: REF | e ) (Tuple Status View)
handleGetFacesAction _ _ = pure $ Tuple statusNotFound ErrorView

handleCreateFaceAction
  :: forall e. Ref State
  -> Query
  -> Body
  -> Eff ( now :: NOW, ref :: REF | e ) (Tuple Status View)
handleCreateFaceAction ref query body = do
  users <- getUserStatuses ref
  case auth users query of
    (Left r) -> pure r
    (Right userStatus@(UserStatus { user: user@(User { id }) })) -> do
      case params body of
        (Left r) -> pure r
        (Right body') -> do
          result <- updateUser ref user body'
          pure $ case result of
            Nothing -> Tuple statusNotFound NotFoundView
            (Just _) -> Tuple statusOK OKView
  where
    auth :: Array UserStatus -> Query -> Either (Tuple Status View) UserStatus
    auth users query =
      maybe
        (Left $ Tuple statusForbidden ForbiddenView)
        Right
        (credentials query >>= (flip authenticatedUser users))
    params :: String -> Either (Tuple Status View) UpdateUserBody
    params body =
      either
        (const $ Left $ Tuple statusBadRequest BadRequestView)
        Right
        ((jsonParser body >>= decodeJson) :: Either String UpdateUserBody)

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
doAction ref (Just { action: GetFaces, query }) =
  handleGetFacesAction ref query
doAction ref (Just { action: CreateFace, body, query }) =
  handleCreateFaceAction ref query body
doAction _ _ =
  handleDefaultAction
