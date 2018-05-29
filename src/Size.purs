module Size
  ( getColumns
  , getSize
  ) where

import Data.Int as Int
import Prelude (div, min, (&&), (*), (+), (-), (<=))

type Rectangle = { w :: Int, h :: Int }

getColumns :: Int -> Rectangle -> Int
getColumns 0 _ = 0
getColumns 1 { w, h } = 1
getColumns count r@{ w, h } =
  let
    cols = getColumns (count - 1) r
    size = getSize cols r
    rows = Int.floor (Int.toNumber ((count - 1) `div` cols + 1))
  in
    if Int.toNumber rows * size <= Int.toNumber h
      then cols
      else cols + 1

getSize :: Int -> Rectangle -> Number
getSize 0 _ = 0.0
getSize 1 { w, h } = Int.toNumber (min w h)
getSize columns { w } = (Int.toNumber w) `div` (Int.toNumber columns)

isValid :: Int -> Int -> Rectangle -> Boolean
isValid columns count { w, h } =
  let
    size = w `div` columns
    rows = (count - 1) `div` columns + 1
  in
    size * columns <= w && size * rows <= h
