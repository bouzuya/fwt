module View (View(..)) where

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Argonaut as Json
import Data.Array (catMaybes)
import Data.FaceWithTime (FaceWithTime)
import Data.FaceWithTime as FaceWithTime
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Show (class Show)
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Data.User (User)
import Data.User as User
import Data.UserStatus (UserStatus(..))
import Halogen.HTML (ClassName(..), HTML(..), PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.DOM.StringRenderer (render)

data View
  = BadRequestView
  | ErrorView
  | FaceView FaceWithTime
  | FacesView (Array UserStatus)
  | ForbiddenView
  | IndexView
  | NotFoundView
  | OKView
  | UsersView (Array UserStatus)

faces :: Array UserStatus -> Array FaceWithTime
faces userStatuses = catMaybes $ (\(UserStatus { fwt }) -> fwt) <$> userStatuses

users :: Array UserStatus -> Array User
users userStatuses = (\(UserStatus { user }) -> user) <$> userStatuses

instance encodeJsonView :: EncodeJson View where
  encodeJson BadRequestView =
    Json.fromObject $
      StrMap.fromFoldable
        [ Tuple "status" $ Json.fromString "BadRequest" ]
  encodeJson ErrorView =
    Json.fromObject $
      StrMap.fromFoldable
        [ Tuple "status" $ Json.fromString "ERROR" ]
  encodeJson (FaceView fwt) =
    encodeJson fwt -- with secret
  encodeJson (FacesView xs) =
    Json.fromArray $
      encodeJson <$> FaceWithTime.toClient <$> faces xs
  encodeJson ForbiddenView =
    Json.fromObject $
      StrMap.fromFoldable
        [ Tuple "status" $ Json.fromString "Forbidden" ]
  encodeJson IndexView =
    Json.fromObject $
      StrMap.fromFoldable
        [ Tuple "status" $ Json.fromString "Index" ]
  encodeJson NotFoundView =
    Json.fromObject $
      StrMap.fromFoldable
        [ Tuple "status" $ Json.fromString "NotFound" ]
  encodeJson OKView =
    Json.fromObject $
      StrMap.fromFoldable
        [ Tuple "status" $ Json.fromString "OK" ]
  encodeJson (UsersView xs) =
    Json.fromArray $ encodeJson <$> User.toClient <$> users xs

indexView :: PlainHTML
indexView = HH.html []
  [ HH.head []
    [ HH.meta [ HP.charset "UTF-8" ]
    , HH.title [] [ HH.text "fwt" ]
    , HH.link
      [ HP.href "/styles/index.css"
      , HP.rel "stylesheet"
      ]
    ]
  , HH.body []
    [ HH.div []
      [ HH.header [] [ HH.h1 [] [ HH.text "fwt" ] ]
      , HH.div [ HP.classes [ ClassName "body" ] ] []
      , HH.footer []
        [ HH.address []
          [ HH.span [ HP.classes [ ClassName "source-code" ] ]
            [ HH.span [ HP.classes [ ClassName "label" ] ]
              [ HH.text "GitHub" ]
            , HH.span [ HP.classes [ ClassName "value" ] ]
              [ HH.a [ HP.href "https://github.com/bouzuya/fwt" ]
                [ HH.text "bouzuya/fwt" ]
              ]
            ]
          , HH.span [ HP.classes [ ClassName "author" ] ]
            [ HH.span [ HP.classes [ ClassName "label" ] ]
              [ HH.text "Author" ]
            , HH.span [ HP.classes [ ClassName "value" ] ]
              [ HH.a [ HP.href "https://bouzuya.net/" ] [ HH.text "bouzuya" ] ]
            ]
          ]
        ]
      ]
    , HH.script [ HP.src "/scripts/index.js" ] []
    ]
  ]

renderWidget :: forall w. w -> String
renderWidget _ = ""

instance showView :: Show View where
  show IndexView =
    let (HTML vdom) = indexView in
    render renderWidget vdom
  show x = Json.stringify $ encodeJson x
