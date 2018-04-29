module View (View(..)) where

import Data.Argonaut (class EncodeJson, encodeJson, fromArray, fromObject, fromString, stringify)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Show (class Show)
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Data.UserStatus (UserStatus(..))
import Data.UserView (UserView(..))
import Halogen.HTML (HTML(..), PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.DOM.StringRenderer (render)
import View.UserStatusView (UserStatusView(..))

data View
  = BadRequestView
  | ErrorView
  | FacesView (Array UserStatus)
  | ForbiddenView
  | IndexView
  | NotFoundView
  | OKView
  | UsersView (Array UserStatus)

instance encodeJsonView :: EncodeJson View where
  encodeJson BadRequestView = fromObject $ StrMap.fromFoldable
    [ Tuple "status" $ fromString "BadRequest" ]
  encodeJson ErrorView = fromObject $ StrMap.fromFoldable
    [ Tuple "status" $ fromString "ERROR" ]
  encodeJson (FacesView xs) =
    fromArray $ encodeJson <$> UserStatusView <$> xs
  encodeJson ForbiddenView = fromObject $ StrMap.fromFoldable
    [ Tuple "status" $ fromString "Forbidden" ]
  encodeJson IndexView = fromObject $ StrMap.fromFoldable
    [ Tuple "status" $ fromString "Index" ]
  encodeJson NotFoundView = fromObject $ StrMap.fromFoldable
    [ Tuple "status" $ fromString "NotFound" ]
  encodeJson OKView = fromObject $ StrMap.fromFoldable
    [ Tuple "status" $ fromString "OK" ]
  encodeJson (UsersView xs) =
    fromArray $ encodeJson <$> (\(UserStatus { user }) -> UserView user) <$> xs

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
      , HH.div [] [ HH.p [] [ HH.text "index" ] ]
      , HH.footer []
        [ HH.address []
          [ HH.a [ HP.href "https://bouzuya.net/" ] [ HH.text "bouzuya" ] ]
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
  show x = stringify $ encodeJson x
