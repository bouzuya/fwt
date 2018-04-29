module Data.URL
  ( URL(..)
  , url
  , urlWithQuery
  ) where

import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, fromString)
import Data.Foldable (intercalate)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (class Show)
import Data.Tuple (Tuple(..))
import Global (encodeURIComponent)

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

urlWithQuery :: String -> Array (Tuple String (Maybe String)) -> Maybe URL
urlWithQuery url' query = Just <$> URL $ url' <> "?" <> qs
  where
    f (Tuple k Nothing) = encodeURIComponent k <> "="
    f (Tuple k (Just v)) = encodeURIComponent k <> "=" <> encodeURIComponent v
    qs = intercalate "&" $ f <$> query
