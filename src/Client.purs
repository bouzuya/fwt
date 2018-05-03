module Client
  ( main
  ) where

import Component.App (app)
import Control.Bind (bind, pure)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Error.Class (throwError)
import DOM (DOM)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Maybe (maybe)
import Data.Unit (Unit, unit)
import Graphics.Canvas (CANVAS)
import Halogen (liftEff)
import Halogen.Aff (awaitLoad, runHalogenAff, selectElement)
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
  awaitLoad
  appMaybe <- selectElement (QuerySelector ".app")
  appElement <- maybe (throwError (error "Could not find .app")) pure appMaybe
  runUI app unit appElement
