module Client
  ( main
  ) where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Aff (Aff)
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
import Network.HTTP.Affjax as AX
import Prelude (discard, not, ($))

type State =
  { face :: String
  , isOn :: Boolean
  , label :: String
  , loading :: Boolean
  , result :: Maybe String
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

button :: forall e. H.Component HH.HTML Query Input Message (Aff (ajax :: AX.AJAX | e))
button =
  H.component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: Input -> State
    initialState label =
      { face: ""
      , isOn: false
      , label
      , loading: false
      , result: Nothing
      , userId: "bouzuya"
      }

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
        , HH.span []
          [ if state.loading then HH.text "LOADING..." else HH.text ""
          ]
        ]

    eval :: Query ~> H.ComponentDSL State Query Message (Aff (ajax :: AX.AJAX | e))
    eval = case _ of
      Request next -> do
        { face, userId } <- H.get
        H.modify (_ { loading = true })
        response <- H.liftAff $ AX.put ("/users/" <> userId) ("{\"face\":\"" <> face <> "\"}")
        H.modify (_ { loading = false, result = Just response.response })
        pure next
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

main :: forall e. Eff ( ajax :: AX.AJAX
                      , avar :: AVAR
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
