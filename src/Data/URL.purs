module Data.URL (URL, url) where

import Data.Argonaut (class EncodeJson, fromString)
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Just))
import Data.Show (class Show)

newtype URL = URL String

instance encodeJsonURL :: EncodeJson URL where
  encodeJson (URL u) = fromString u

instance showURL :: Show URL where
  show (URL u) = u

url :: String -> Maybe URL
url = Just <$> URL -- TODO
