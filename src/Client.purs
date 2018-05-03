module Client
  ( main
  ) where

import Component.Button (button)
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Data.Unit (Unit, unit)
import Graphics.Canvas (CANVAS)
import Halogen (liftEff)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import Prelude (discard, ($))
import Video (MEDIA, VIDEO)

main :: forall e. Eff ( ajax :: AJAX
                      , avar :: AVAR
                      , canvas :: CANVAS
                      , console :: CONSOLE
                      , dom :: DOM
                      , exception :: EXCEPTION
                      , media :: MEDIA
                      , timer :: TIMER
                      , ref :: REF
                      , video :: VIDEO
                      | e
                      )
                      Unit
main = runHalogenAff do
  liftEff $ log "Hello!"
  body <- awaitBody
  runUI button unit body
