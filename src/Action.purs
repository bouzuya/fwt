module Action (doAction) where

import Control.Applicative (pure)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, readRef)
import Data.Array (findIndex, modifyAt)
import Data.FaceWithTime (FaceWithTime, fwt)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Data.URL (url)
import Data.User (User(User))
import Hyper.Status (Status, statusNotFound, statusOK)
import Prelude (bind, discard, (==))
import Route (Action(..))
import View (View(..))

type State = { users :: Array { user :: User, fwt :: Maybe FaceWithTime } }

modify' :: forall a. (a -> Boolean) -> (a -> a) -> Array a -> Maybe (Array a)
modify' f g xs = do
  index <- findIndex f xs
  modifyAt index g xs

doAction
  :: forall e. Ref State
  -> Maybe Action
  -> Eff ( now :: NOW, ref :: REF | e ) (Tuple Status View)
doAction _ (Just GetIndex) = pure $ Tuple statusOK OKView
doAction ref (Just GetUsers) = do
  { users } <- readRef ref
  pure $ Tuple statusOK $ UsersView users
doAction ref (Just (UpdateUser id')) = do
  time <- now
  { users } <- readRef ref
  case
    modify'
      (\({ user: (User { id }) }) -> show id == id')
      (\({ user }) ->
        { user
        , fwt: (\face -> fwt { face, time }) <$> url "http://example.com"
        })
      users of
    Nothing -> pure $ Tuple statusNotFound NotFoundView
    (Just newUsers) -> do
      modifyRef ref (\_ -> { users: newUsers })
      pure $ Tuple statusOK OKView
doAction _ _ = pure $ Tuple statusNotFound ErrorView
