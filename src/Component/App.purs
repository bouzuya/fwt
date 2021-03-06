module Component.App
  ( Query
  , Message
  , app
  ) where

import Capture (snapshot, start, stop)
import Control.Applicative (pure, void, (<$>))
import Control.Bind (bind, (=<<))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER, clearTimeout, setTimeout)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes as EventType
import DOM.HTML.Types (windowToEventTarget)
import DOM.HTML.Window (innerHeight, innerWidth)
import Data.Array (filter, find)
import Data.Array as Array
import Data.ClientFaceWithTime (ClientFaceWithTime(..))
import Data.ClientUser (ClientUser(..))
import Data.Foldable (length)
import Data.Function (const)
import Data.Int as Math
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.NaturalTransformation (type (~>))
import Data.Semigroup ((<>))
import Data.StrMap (lookup)
import Data.Unit (Unit, unit)
import Graphics.Canvas (CANVAS)
import Halogen (AttrName(..), ClassName(..), SubscribeStatus, lift)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as HES
import Network.HTTP.Affjax as AX
import Prelude (discard, eq, not, show, ($), (==), (>))
import Request as Request
import Size (getColumns, getSize)
import Video (MEDIA, VIDEO)

type ClientUserStatus =
  { fwt :: Maybe ClientFaceWithTime
  , user :: ClientUser
  }

type State =
  { isCapturing :: Boolean
  , loading :: Boolean
  , password :: String
  , signedInUser :: Maybe { password :: String, userId :: String }
  , userId :: String
  , userStatuses :: Array ClientUserStatus
  , windowSize :: { w :: Int, h :: Int }
  }

data Query a
  = SignIn a
  | SignOut a
  | Snapshot a
  | Tick (SubscribeStatus -> a)
  | UpdatePassword String a
  | UpdateUserId String a
  | WindowResized a

type Message = Unit
type Input = Unit

mergeFaces
  :: Array ClientFaceWithTime -> Array ClientUserStatus -> Array ClientUserStatus
mergeFaces faces userStatuses =
  (\s -> s { fwt = (fwt s.user) }) <$> userStatuses
  where
    fwt (ClientUser { id }) =
      find (\(ClientFaceWithTime { userId }) -> userId == id) faces

snapshot'
  :: forall e
  . H.ComponentDSL
      State
      Query
      Message
      (Aff
        ( ajax :: AX.AJAX
        , avar :: AVAR
        , canvas :: CANVAS
        , console :: CONSOLE
        , media :: MEDIA
        , timer :: TIMER
        , video :: VIDEO
        | e
        )
      )
      Unit
snapshot' = (maybe (H.modify (_ { loading = false })) pure) =<< runMaybeT do
  face <- MaybeT $ H.liftEff $ snapshot
  { password, userId } <- H.get
  H.modify (_ { loading = true })
  map <- MaybeT $ lift $ Request.createFace { password, userId } face
  let secret = lookup "secret" map
  H.modify (_ { loading = false })
  -- get faces
  { userStatuses } <- H.get
  H.modify (_ { loading = true })
  faces <- MaybeT $ lift $ Request.getFaces { password, secret, userId }
  let
    unknownFaces =
      filter
        (\(ClientFaceWithTime { userId: u' }) ->
          eq 0 $ length $ filter
            (\({ user: (ClientUser { id: u }) }) -> u == u')
            userStatuses
        )
        faces
  H.modify
    (\s -> s
      { loading = false
      , userStatuses =
          if length unknownFaces > 0
          then []
          else mergeFaces faces s.userStatuses
      })
  pure unit

app
  :: forall e
  . H.Component
      HH.HTML
      Query
      Input
      Message
      (Aff
        ( ajax :: AX.AJAX
        , avar :: AVAR
        , canvas :: CANVAS
        , console :: CONSOLE
        , dom :: DOM
        , media :: MEDIA
        , timer :: TIMER
        , video :: VIDEO
        | e
        )
      )
app =
  H.component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: Input -> State
    initialState _ =
      { isCapturing: false
      , loading: false
      , password: ""
      , signedInUser: Nothing
      , userId: ""
      , userStatuses: []
      , windowSize: { w: 640, h: 480 }
      }

    render :: State -> H.ComponentHTML Query
    render state =
      let
        isMe ({ user: ClientUser { name } }) = name == state.userId
        me = find isMe state.userStatuses
        others = filter (not $ isMe) state.userStatuses
        wSize = state.windowSize
        columns = getColumns (Array.length state.userStatuses) wSize
        size = show $ Math.floor $ getSize columns wSize
      in
        HH.div
        [ HP.classes
          ([ ClassName "app" ] <>
            if isNothing state.signedInUser
              then []
              else [ ClassName "is-signed-in" ]
          )
        ]
        [ HH.div [ HP.classes [ ClassName "credentials" ] ]
          [ HH.label []
            [ HH.span [ HP.classes [ ClassName "label" ] ]
              [ HH.text "user id" ]
            , HH.span [ HP.classes [ ClassName "value" ] ]
              [ HH.input
                [ HE.onValueChange (HE.input UpdateUserId)
                , HP.readOnly (not $ isNothing state.signedInUser)
                , HP.type_ HP.InputText
                , HP.value state.userId
                ]
              ]
            ]
          , HH.label []
            [ HH.span [ HP.classes [ ClassName "label" ] ]
              [ HH.text "password" ]
            , HH.span [ HP.classes [ ClassName "value" ] ]
              [ HH.input
                [ HE.onValueChange (HE.input UpdatePassword)
                , HP.readOnly (not $ isNothing state.signedInUser)
                , HP.type_ HP.InputPassword
                , HP.value state.password
                ]
              ]
            ]
          , if isNothing state.signedInUser
              then
                HH.button
                [ HE.onClick (HE.input_ SignIn) ]
                [ HH.text "SIGN IN" ]
              else
                HH.button
                [ HE.onClick (HE.input_ SignOut) ]
                [ HH.text "SIGN OUT" ]
          ]
        , if isNothing state.signedInUser
            then
              HH.ul [] []
            else
              HH.ul [] $
              [ HH.li
                [ HP.attr
                    (AttrName "style")
                    ("width: " <> size <> "px; height: " <> size <> "px")
                ] $
                [ HH.div
                  [ HP.classes [ ClassName "capture" ] ] $
                  [ HH.div [ HP.classes [ ClassName "controls" ] ]
                    if state.isCapturing
                    then
                      [ HH.div
                        [ HP.classes [ ClassName "snapshot-button" ]
                        , HE.onClick (HE.input_ Snapshot)
                        ]
                        [ HH.text "SNAPSHOT" ]
                      ]
                    else
                      []
                  , HH.video
                    [ HP.autoplay true
                    , HP.id_ "video"
                    ] []
                  , HH.canvas
                    [ HP.height 640
                    , HP.id_ "canvas"
                    , HP.width 640
                    ]
                  ]
                ] <> (maybe [] (\m -> [renderUserStatus m]) me)
              ] <>
              ((\user ->
                HH.li
                [ HP.attr
                    (AttrName "style")
                    ("width: " <> size <> "px; height: " <> size <> "px")
                ] $ [renderUserStatus user]) <$> others
              )
        , HH.span
          [ HP.classes
            ([ ClassName "throbber" ] <>
              if state.loading
                then [ ClassName "is-show" ]
                else []
            )
          ]
          []
        ]

    renderUserStatus :: ClientUserStatus -> H.ComponentHTML Query
    renderUserStatus
      { fwt
      , user: (ClientUser { id: userId, name: userName })
      } =
      HH.div
      [ HP.class_ $ ClassName "user-status" ]
      [ lv "user-id" "UserId" $ HH.text $ show userId
      , lv "user-name" "UserName" $ HH.text $ userName
      , lv "face" "Face" $
        HH.img
        [ HP.src (maybe "" (\(ClientFaceWithTime { face }) -> show face) fwt)
        ]
      , lv "time" "Time" $
        HH.text (maybe "" (\(ClientFaceWithTime { time }) -> show time) fwt)
      ]
      where
        lv c l v =
          HH.div [ HP.classes [ ClassName c ] ]
            [ HH.div [ HP.classes [ ClassName "label" ] ] [ HH.text l ]
            , HH.div [ HP.classes [ ClassName "value" ] ] [ v ]
            ]

    eval
      :: Query
      ~> H.ComponentDSL
          State
          Query
          Message
          (Aff
            ( ajax :: AX.AJAX
            , avar :: AVAR
            , canvas :: CANVAS
            , console :: CONSOLE
            , dom :: DOM
            , media :: MEDIA
            , timer :: TIMER
            , video :: VIDEO
            | e
            )
          )
    eval = case _ of
      SignIn next -> do
        -- resize
        w <- H.liftEff window
        width <- H.liftEff $ innerWidth w
        height <- H.liftEff $ innerHeight w
        H.modify (\s -> s { windowSize = { w: width, h: height } })
        -- sign in
        { password, userId } <- H.get
        H.modify (_ { loading = true })
        usersMaybe <- lift $ Request.getUsers { password, userId }
        case usersMaybe of
          Nothing -> do
            H.modify (_ { loading = false })
            pure next
          (Just users) -> do
            H.modify
              (_ { loading = false
                 , signedInUser = Just { password, userId }
                 , userStatuses = (\user -> { fwt: Nothing, user }) <$> users
                 })
            -- start capture
            _ <- lift $ start
            H.modify (_ { isCapturing = true })
            H.subscribe $
              HES.eventSource_'
                (\e -> do
                  id <- setTimeout 1000 e
                  pure $ void $ clearTimeout id
                )
                (H.request Tick)
            H.subscribe $
              HES.eventSource'
                (\f -> do
                  w <- window
                  let listener = eventListener f
                  _ <-
                    addEventListener
                      EventType.resize
                      listener
                      false
                      (windowToEventTarget w)
                  pure $
                    removeEventListener
                      EventType.resize
                      listener
                      false
                      (windowToEventTarget w)
                )
                (\e -> pure $ WindowResized HES.Listening)
            pure next
      SignOut next -> do
        _ <- H.liftEff $ stop
        H.modify (_ { isCapturing = false, signedInUser = Nothing })
        pure next
      Snapshot next -> do
        snapshot'
        pure next
      Tick next -> do
        { isCapturing } <- H.get
        if isCapturing
          then do
            snapshot'
            H.subscribe $
              HES.eventSource_'
                (\e -> do
                  id <- setTimeout 300000 e
                  pure $ void $ clearTimeout id
                )
                (H.request Tick)
          else pure unit
        pure $ next HES.Done
      UpdatePassword password next -> do
        H.modify (\s -> s { password = password })
        pure next
      UpdateUserId userId next -> do
        H.modify (\s -> s { userId = userId })
        pure next
      WindowResized next -> do
        w <- H.liftEff window
        width <- H.liftEff $ innerWidth w
        height <- H.liftEff $ innerHeight w
        H.modify (\s -> s { windowSize = { w: width, h: height } })
        pure next
