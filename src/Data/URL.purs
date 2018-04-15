module Data.URL (URL, url) where

import Data.Functor ((<$>))
import Data.Maybe (Maybe(Just))
import Data.Show (class Show)

newtype URL = URL String

instance showURL :: Show URL where
  show (URL u) = u

url :: String -> Maybe URL
url = Just <$> URL -- TODO
