module Client
  ( main
  ) where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import Data.Function (const)
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (type (~>))
import Data.Unit (Unit, unit)
import Halogen (liftEff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Prelude (discard, not, ($))

type State = Boolean

data Query a
  = Toggle a
  | IsOn (Boolean -> a)

data Message = Toggled Boolean

button :: forall m. H.Component HH.HTML Query Unit Message m
button =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: State
    initialState = false

    render :: State -> H.ComponentHTML Query
    render state =
      let
        label = if state then "On" else "Off"
      in
        HH.button
          [ HP.title label
          , HE.onClick (HE.input_ Toggle)
          ]
          [ HH.text label ]

    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of
      Toggle next -> do
        state <- H.get
        let nextState = not state
        H.put nextState
        H.raise $ Toggled nextState
        pure next
      IsOn reply -> do
        state <- H.get
        pure (reply state)

main :: forall e. Eff ( avar :: AVAR
                      , console :: CONSOLE
                      , dom :: DOM
                      , exception :: EXCEPTION
                      , ref :: REF
                      | e
                      )
                      Unit
main = HA.runHalogenAff do
  liftEff $ log "Hello!"
  body <- HA.awaitBody
  runUI button unit body
