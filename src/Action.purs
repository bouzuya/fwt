module Action (doAction) where

import Control.Applicative (pure)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, readRef)
import Data.Argonaut (class DecodeJson, decodeJson, jsonParser, (.?))
import Data.Array (findIndex, modifyAt)
import Data.Either (Either(..), either)
import Data.FaceWithTime (FaceWithTime(..))
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
type Credentials' = { password :: String, secret :: String, userId :: String }
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

credentials' :: Query -> Maybe Credentials'
credentials' query = do
  let lookup k a = maybe Nothing snd $ find (\t -> fst t == k) a
  password <- lookup "password" query
  secret <- lookup "secret" query
  userId <- lookup "user_id" query
  pure { password, secret, userId }

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

authenticatedUser'
  :: Credentials'
  -> Array UserStatus
  -> Maybe UserStatus
authenticatedUser' { password, secret, userId } users =
  find
    (\(UserStatus
      { fwt
      , user: (User { id: (UserId uuid), password: p })
      }) ->
        case fwt of
          Nothing -> false
          (Just (FaceWithTime { secret: s })) ->
            show uuid == userId && p == password && s == secret
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
      (\(UserStatus { user: user' }) ->
          UserStatus
            { user: user'
            , fwt: (\f -> FaceWithTime { face: f, secret: "abc", time }) <$> url face -- TODO: generate secret
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
    authenticate q u = maybe false (const true) $
      credentials q >>= (flip authenticatedUser u)

handleGetFacesAction
  :: forall e. Ref State
  -> Query
  -> Eff ( ref :: REF | e ) (Tuple Status View)
handleGetFacesAction ref query =  do
  users <- getUserStatuses ref
  pure $ if authenticate query users
    then Tuple statusOK (FacesView users)
    else Tuple statusForbidden ForbiddenView
  where
    authenticate q u = maybe false (const true) $
      credentials q >>= (flip authenticatedUser u)

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
    auth users q =
      maybe
        (Left $ Tuple statusForbidden ForbiddenView)
        Right
        (credentials q >>= (flip authenticatedUser users))
    params :: String -> Either (Tuple Status View) UpdateUserBody
    params b =
      either
        (const $ Left $ Tuple statusBadRequest BadRequestView)
        Right
        ((jsonParser b >>= decodeJson) :: Either String UpdateUserBody)

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
