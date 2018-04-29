module Client
  ( main
  ) where

import Capture (snapshot, start, stop)
import Control.Applicative (pure, (<$>))
import Control.Bind (bind, (>>=))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import Data.Argonaut (decodeJson, jsonParser)
import Data.Array (filter, find)
import Data.ClientFaceWithTime (ClientFaceWithTime(..))
import Data.ClientUser (ClientUser(..))
import Data.Either (Either(..), either)
import Data.Foldable (length)
import Data.Function (const, id)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.NaturalTransformation (type (~>))
import Data.Semigroup ((<>))
import Data.StrMap (StrMap, lookup)
import Data.Tuple (Tuple(..))
import Data.URL (URL(..), parseUrlWithQuery)
import Data.UUID (parseUUID)
import Data.Unit (Unit)
import Data.UserId (UserId(..))
import Graphics.Canvas (CANVAS)
import Halogen (ClassName(..), lift, liftEff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax as AX
import Prelude (discard, eq, not, show, ($), (==), (>))
import Video (MEDIA, VIDEO)

type ClientUserStatus =
  { fwt :: Maybe ClientFaceWithTime
  , user :: ClientUser
  }

type State =
  { isCapturing :: Boolean
  , label :: String
  , loading :: Boolean
  , password :: String
  , secret :: Maybe String
  , signedInUser :: Maybe { password :: String, userId :: String }
  , userId :: String
  , userStatuses :: Array ClientUserStatus
  }

data Query a
  = LoadRequest a
  | SignIn a
  | Snapshot a
  | StartCapture a
  | StopCapture a
  | UpdatePassword String a
  | UpdateUserId String a

data Message = Toggled Boolean
type Input = String

mergeFaces
  :: Array ClientFaceWithTime -> Array ClientUserStatus -> Array ClientUserStatus
mergeFaces faces userStatuses =
  (\s -> s { fwt = (fwt s.user) }) <$> userStatuses
  where
    fwt (ClientUser { id }) =
      find (\(ClientFaceWithTime { userId }) -> userId == id) faces

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
      , password: "pass1"
      , secret: Nothing
      , signedInUser: Nothing
      , userId: "user1"
      , userStatuses: []
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
        isMe ({ user: ClientUser { id: (UserId id') } }) =
          maybe false (eq id') $ parseUUID state.userId
        me = find isMe state.userStatuses
        others = filter (not $ isMe) state.userStatuses
        userStatus
          { fwt
          , user: (ClientUser { id: userId, name: userName })
          } =
            [ landv "user-id" "UserId" $ show userId
            , landv "user-name" "UserName" $ userName
            , landimg "face" "Face" $ maybe "" (\(ClientFaceWithTime { face }) -> show face) fwt
            , landv "time" "Time" $ maybe "" (\(ClientFaceWithTime { time }) -> show time) fwt
            ]
        renderMe me' =
          HH.div
          [ HP.classes [ ClassName "capture" ] ] $
          [ HH.div [ HP.classes [ ClassName "controls" ] ]
            if state.isCapturing
            then
              [ HH.div
                [ HP.classes [ ClassName "stop-button" ]
                , HE.onClick (HE.input_ StopCapture)
                ]
                [ HH.text "STOP" ]
              , HH.div
                [ HP.classes [ ClassName "snapshot-button" ]
                , HE.onClick (HE.input_ Snapshot)
                ]
                [ HH.text "SNAPSHOT" ]
              ]
            else
              [
                HH.div
                [ HP.classes [ ClassName "start-button" ]
                , HE.onClick (HE.input_ StartCapture)
                ]
                [ HH.text "START" ]
              ]
          , HH.video
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
          ] <> (maybe [] userStatus me')
      in
        if isNothing state.signedInUser
          then
            HH.div []
            [ HH.label []
              [ HH.span [] [ HH.text "user id" ]
              , HH.input
                [ HE.onValueChange (HE.input UpdateUserId)
                , HP.type_ HP.InputText
                , HP.value state.userId
                ]
              ]
            , HH.label []
              [ HH.span [] [ HH.text "password" ]
              , HH.input
                [ HE.onValueChange (HE.input UpdatePassword)
                , HP.type_ HP.InputPassword
                , HP.value state.password
                ]
              ]
            , HH.button
              [ HE.onClick (HE.input_ SignIn)
              ]
              [ HH.text "SIGN IN" ]
            , HH.span []
              [ if state.loading then HH.text "LOADING..." else HH.text ""
              ]
            ]
          else
            HH.div []
            [ HH.button
              [ HE.onClick (HE.input_ LoadRequest)
              ]
              [ HH.text "LOAD" ]
            , HH.ul [] $
              [ HH.li [] [ renderMe me ]
              ] <>
              ( (\user ->
                  HH.li []
                  [ HH.div [ HP.class_ $ ClassName "user-status" ] (userStatus user)
                  ]
                ) <$> others
              )
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
        { password, secret, userId, userStatuses } <- H.get
        H.modify (_ { loading = true })
        let params =
              [ Tuple "password" $ Just password
              , Tuple "secret" $ secret
              , Tuple "user_id" $ Just userId
              ]
        case (parseUrlWithQuery "/faces" params) of
          Nothing -> pure next
          (Just (URL url)) -> do
            response <- H.liftAff $ AX.get url
            let faces :: Array ClientFaceWithTime
                faces = either (const []) id $ decodeJson response.response
                unknownFaces =
                  filter
                    (\(ClientFaceWithTime { userId }) ->
                      eq 0 $ length $ filter
                        (\({ user: (ClientUser { id: u }) }) -> u == userId)
                        userStatuses
                    )
                    faces
            if length unknownFaces > 0
                then do
                  pure next -- TODO
                else do
                  H.modify
                    (\s -> s
                      { loading = false
                      , userStatuses = mergeFaces faces s.userStatuses
                      })
                  pure next
      SignIn next -> do
        { password, userId } <- H.get
        H.modify (_ { loading = true })
        let params =
              [ Tuple "password" $ Just password
              , Tuple "user_id" $ Just userId
              ]
        case (parseUrlWithQuery "/users" params) of
          Nothing -> pure next
          (Just (URL url)) -> do
            response <- H.liftAff $ AX.get url
            let users :: Array ClientUser
                users = either (const []) id $ decodeJson response.response
            H.modify
              (_ { loading = false
                 , signedInUser = Just { password, userId }
                 , userStatuses = (\user -> { fwt: Nothing, user }) <$> users
                 })
            pure next
      Snapshot next -> do
        dataUrl <- H.liftEff $ snapshot
        case dataUrl of
          Nothing -> pure next
          (Just face) -> do
            { password, userId } <- H.get
            H.modify (_ { loading = true })
            let params =
                  [ Tuple "password" $ Just password
                  , Tuple "user_id" $ Just userId
                  ]
            case (parseUrlWithQuery "/faces" params) of
              Nothing -> pure next
              (Just (URL url)) -> do
                response <- H.liftAff $ AX.post url ("{\"face\":\"" <> face <> "\"}")
                let e =
                      (jsonParser response.response >>= decodeJson) :: Either String (StrMap String)
                let m =
                      case e of
                        (Left _) -> Nothing
                        (Right map) -> lookup "secret" map
                H.modify (_ { loading = false, secret = m })
                pure next
      StartCapture next -> do
        _ <- lift $ start
        H.modify (_ { isCapturing = true })
        pure next
      StopCapture next -> do
        _ <- H.liftEff $ stop
        H.modify (_ { isCapturing = false })
        pure next
      UpdatePassword password next -> do
        H.modify (\s -> s { password = password })
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
