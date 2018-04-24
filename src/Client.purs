module Client
  ( main
  ) where

import Capture (snapshot, start, stop)
import Control.Applicative (pure, (<$>))
import Control.Bind (bind)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import Data.Argonaut (decodeJson)
import Data.Array (length)
import Data.Either (either)
import Data.FaceWithTime (FaceWithTime(..), toIso8601)
import Data.Function (const, id)
import Data.Maybe (Maybe(..), maybe)
import Data.NaturalTransformation (type (~>))
import Data.Semigroup ((<>))
import Data.Unit (Unit)
import Data.User (User(..))
import Data.UserStatus (UserStatus(..))
import Graphics.Canvas (CANVAS)
import Halogen (ClassName(..), lift, liftEff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax as AX
import Prelude (discard, show, ($))
import Video (MEDIA, VIDEO)

type State =
  { isCapturing :: Boolean
  , label :: String
  , loading :: Boolean
  , result :: Maybe String
  , userId :: String
  , users :: Array UserStatus
  }

data Query a
  = LoadRequest a
  | Snapshot a
  | StartCapture a
  | StopCapture a
  | UpdateUserId String a

data Message = Toggled Boolean
type Input = String

button
  :: forall e
  . H.Component
      HH.HTML
      Query
      Input
      Message
      (Aff
        ( ajax :: AX.AJAX
        , canvas :: CANVAS
        , media :: MEDIA
        , video :: VIDEO
        | e
        )
      )
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
      { isCapturing: false
      , label
      , loading: false
      , result: Nothing
      , userId: "bouzuya"
      , users: []
      }

    render :: State -> H.ComponentHTML Query
    render state =
      let
        landv c l v =
          HH.div [ HP.class_ $ ClassName c ]
            [ HH.div [ HP.class_ $ ClassName "label"] [ HH.text l ]
            , HH.div [ HP.class_ $ ClassName "value"] [ HH.text v ]
            ]
        landimg c l v =
          HH.div [ HP.class_ $ ClassName c ]
            [ HH.div [ HP.class_ $ ClassName "label"] [ HH.text l ]
            , HH.div [ HP.class_ $ ClassName "value"] [ HH.img [ HP.src v ] ]
            ]
        renderUser
          ( UserStatus
            { fwt
            , user: (User { id: userId, name: userName })
            }
          ) =
            HH.div [ HP.class_ $ ClassName "user-status" ]
            [ landv "user-id" "UserId" $ show userId
            , landv "user-name" "UserName" $ userName
            , landimg "face" "Face" $ maybe "" (\(FaceWithTime { face }) -> show face) fwt
            , landv "time" "Time" $ maybe "" (\(FaceWithTime { time }) -> toIso8601 time) fwt
            ]
      in
        HH.div []
        [
          HH.label []
          [ HH.span [] [ HH.text "user id" ]
          , HH.input
            [ HE.onValueChange (HE.input UpdateUserId)
            , HP.value state.userId
            ]
          ]
        , HH.button
          [ HE.onClick (HE.input_ LoadRequest)
          ]
          [ HH.text "LOAD" ]
        , HH.span []
          [ if state.loading then HH.text "LOADING..." else HH.text ""
          ]
        , HH.div [ HP.class_ $ ClassName "capture-controls"]
          if state.isCapturing
          then
            [ HH.button
              [ HE.onClick (HE.input_ StopCapture)
              ]
              [ HH.text "STOP" ]
            , HH.button
              [ HE.onClick (HE.input_ Snapshot)
              ]
              [ HH.text "SNAPSHOT" ]
            ]
          else
            [
              HH.button
              [ HE.onClick (HE.input_ StartCapture)
              ]
              [ HH.text "START" ]
            ]
        , HH.span [] [ HH.text $ show $ length state.users]
        , HH.ul [] $
          [ HH.li []
            [ HH.div
              [ HP.classes [ ClassName "capture" ] ]
              [ HH.video
                [ HP.autoplay true
                , HP.height 320
                , HP.id_ "video"
                , HP.width 320
                ] []
              , HH.canvas
                [ HP.height 640
                , HP.id_ "canvas"
                , HP.width 640
                ]
              ]
            ]
          ] <> ((\user -> HH.li [] [renderUser user]) <$> state.users)
        ]

    eval
      :: Query
      ~> H.ComponentDSL
          State
          Query
          Message
          (Aff
            ( ajax :: AX.AJAX
            , canvas :: CANVAS
            , media :: MEDIA
            , video :: VIDEO
            | e
            )
          )
    eval = case _ of
      LoadRequest next -> do
        { userId } <- H.get
        H.modify (_ { loading = true })
        response <- H.liftAff $ AX.get "/users"
        let users = either (const []) id $ decodeJson response.response
        H.modify (_ { loading = false, users = users })
        pure next
      Snapshot next -> do
        dataUrl <- H.liftEff $ snapshot
        case dataUrl of
          Nothing -> pure next
          (Just face) -> do
            { userId } <- H.get
            H.modify (_ { loading = true })
            response <- H.liftAff $ AX.put ("/users/" <> userId) ("{\"face\":\"" <> face <> "\"}")
            H.modify (_ { loading = false, result = Just response.response })
            pure next
      StartCapture next -> do
        _ <- lift $ start
        H.modify (_ { isCapturing = true })
        pure next
      StopCapture next -> do
        _ <- H.liftEff $ stop
        H.modify (_ { isCapturing = false })
        pure next
      UpdateUserId userId next -> do
        H.modify (\s -> s { userId = userId })
        pure next

main :: forall e. Eff ( ajax :: AX.AJAX
                      , avar :: AVAR
                      , canvas :: CANVAS
                      , console :: CONSOLE
                      , dom :: DOM
                      , exception :: EXCEPTION
                      , media :: MEDIA
                      , ref :: REF
                      , video :: VIDEO
                      | e
                      )
                      Unit
main = HA.runHalogenAff do
  liftEff $ log "Hello!"
  body <- HA.awaitBody
  runUI button "Hello!" body
