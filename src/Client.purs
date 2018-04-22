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
import Data.Semigroup ((<>))
import Data.Unit (Unit)
import Halogen (liftEff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Prelude (discard, not, ($))

type State =
  { face :: String
  , isOn :: Boolean
  , label :: String
  , userId :: String
  }

data Query a
  = Request a
  | Toggle a
  | UpdateFace String a
  | UpdateUserId String a
  | IsOn (Boolean -> a)

data Message = Toggled Boolean
type Input = String

button :: forall m. H.Component HH.HTML Query Input Message m
button =
  H.component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: Input -> State
    initialState label = { face: "", isOn: false, label, userId: "bouzuya" }

    render :: State -> H.ComponentHTML Query
    render state =
      let
        label = state.label <> ":" <> if state.isOn then "On" else "Off"
      in
        HH.div []
        [ HH.button
          [ HP.title label
          , HE.onClick (HE.input_ Toggle)
          ]
          [ HH.text label ]
        , HH.label []
          [ HH.span [] [ HH.text "user id" ]
          , HH.input
            [ HE.onValueChange (HE.input UpdateUserId)
            , HP.value state.userId
            ]
          ]
        , HH.label []
          [ HH.span [] [ HH.text "face" ]
          , HH.input
            [ HE.onValueChange (HE.input UpdateFace)
            , HP.value state.face
            ]
          ]
        , HH.button
          [ HE.onClick (HE.input_ Request)
          ]
          [ HH.text "OK" ]
        ]

    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of
      Request next -> pure next -- TODO
      Toggle next -> do
        state <- H.get
        let nextState = state { isOn = not state.isOn }
        H.put nextState
        H.raise $ Toggled nextState.isOn
        pure next
      UpdateFace face next -> do
        H.modify (\s -> s { face = face })
        pure next
      UpdateUserId userId next -> do
        H.modify (\s -> s { userId = userId })
        pure next
      IsOn reply -> do
        state <- H.get
        pure (reply state.isOn)

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
  runUI button "Hello!" body
