module Capture
  ( snapshot
  , start
  , stop
  ) where

import Control.Bind (bind)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Maybe.Trans (MaybeT(..), lift, runMaybeT)
import Control.Promise (toAffE)
import Data.Maybe (Maybe)
import Data.Unit (Unit)
import Graphics.Canvas (CANVAS, canvasToDataURL, drawImageFull, getCanvasElementById, getContext2D)
import Math (floor, min)
import Prelude (($), (-), (/))
import Video (MEDIA, VIDEO, clearSourceObject, getMediaStream, getSourceObject, getVideoElementById, getVideoHeight, getVideoWidth, setSourceObject, stopAllVideoTracks, videoElementToCanvasImageSource)

snapshot
  :: forall e
  . Eff
    ( canvas :: CANVAS
    , video :: VIDEO
    | e
    ) (Maybe String)
snapshot = runMaybeT do
  canvas <- MaybeT $ getCanvasElementById "canvas"
  video <- MaybeT $ getVideoElementById "video"
  context <- lift $ getContext2D canvas
  sw <- lift $ getVideoWidth video
  sh <- lift $ getVideoHeight video
  let ss = min sw sh
      sx = floor $ (sw - ss) / 2.0
      sy = floor $ (sh - ss) / 2.0
      dx = 0.0
      dy = 0.0
      ds = 640.0
  _ <- lift $ drawImageFull
        context
        (videoElementToCanvasImageSource video)
        sx sy ss ss
        dx dy ds ds
  lift $ canvasToDataURL canvas

start
  :: forall e
  . Aff
      ( media :: MEDIA
      , video :: VIDEO
      | e
      )
      (Maybe Unit)
start = runMaybeT do
  video <- MaybeT $ liftEff $ getVideoElementById "video"
  mediaStream <- lift $ toAffE $ getMediaStream
  lift $ liftEff $ setSourceObject mediaStream video

stop
  :: forall e
  . Eff
      ( media :: MEDIA
      , video :: VIDEO
      | e
      )
      (Maybe Unit)
stop = runMaybeT do
  video <- MaybeT $ getVideoElementById "video"
  mediaStream <- MaybeT $ getSourceObject video
  _ <- lift $ stopAllVideoTracks mediaStream
  lift $ clearSourceObject video
