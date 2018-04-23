module Video
  ( MEDIA
  , MediaStream
  , VIDEO
  , VideoElement
  , clearSourceObject
  , getMediaStream
  , getSourceObject
  , getVideoElementById
  , getVideoHeight
  , getVideoWidth
  , setSourceObject
  , stopAllVideoTracks
  , videoElementToCanvasImageSource
  ) where

import Control.Monad.Eff (Eff, kind Effect)
import Control.Promise (Promise)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import Graphics.Canvas (CanvasImageSource)

foreign import data MEDIA :: Effect
foreign import data VIDEO :: Effect

foreign import data MediaStream :: Type
foreign import data VideoElement :: Type

foreign import clearSourceObject
  :: forall e. VideoElement -> Eff (video :: VIDEO | e) Unit

foreign import getMediaStream
  :: forall e. Eff ( media :: MEDIA | e ) (Promise MediaStream)

foreign import getSourceObjectImpl
  :: forall r e
  . Fn3
      VideoElement
      (MediaStream -> r)
      r
      (Eff (video :: VIDEO | e) r)

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

foreign import setSourceObject
  :: forall e. MediaStream -> VideoElement -> Eff (video :: VIDEO | e) Unit

foreign import stopAllVideoTracks
  :: forall e. MediaStream -> Eff (video :: VIDEO | e) Unit

foreign import videoElementToCanvasImageSource :: VideoElement -> CanvasImageSource

getSourceObject
  :: forall e. VideoElement -> Eff (video :: VIDEO | e) (Maybe MediaStream)
getSourceObject e = runFn3 getSourceObjectImpl e Just Nothing

getVideoElementById
  :: forall e. String -> Eff ( video :: VIDEO | e ) (Maybe VideoElement)
getVideoElementById id = runFn3 getVideoElementByIdImpl id Just Nothing
