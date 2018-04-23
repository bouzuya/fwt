module Video
  ( VIDEO
  , VideoElement
  , getVideoElementById
  , getVideoHeight
  , getVideoWidth
  , videoElementToCanvasImageSource
  ) where

import Control.Monad.Eff (Eff, kind Effect)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CanvasImageSource)

foreign import data VIDEO :: Effect

foreign import data VideoElement :: Type

foreign import getVideoElementByIdImpl
  :: forall r e
  . Fn3
      String
      (VideoElement -> r)
      r
      (Eff ( video :: VIDEO | e ) r)

foreign import getVideoHeight
  :: forall e. VideoElement -> Eff (video :: VIDEO | e) Number
foreign import getVideoWidth
  :: forall e. VideoElement -> Eff (video :: VIDEO | e) Number
foreign import videoElementToCanvasImageSource :: VideoElement -> CanvasImageSource

getVideoElementById
  :: forall e. String -> Eff ( video :: VIDEO | e ) (Maybe VideoElement)
getVideoElementById id = runFn3 getVideoElementByIdImpl id Just Nothing
