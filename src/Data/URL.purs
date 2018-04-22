module Data.URL (URL, url) where

import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, fromString)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Just))
import Data.Show (class Show)

newtype URL = URL String

instance decodeJsonURL :: DecodeJson URL where
  decodeJson json = do
    o <- decodeJson json
    pure $ URL o

instance encodeJsonURL :: EncodeJson URL where
  encodeJson (URL u) = fromString u

instance showURL :: Show URL where
  show (URL u) = u

url :: String -> Maybe URL
url = Just <$> URL -- TODO
