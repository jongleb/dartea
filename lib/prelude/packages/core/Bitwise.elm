{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/core -- https://github.com/elm/core
   Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Bitwise exposing
  ( and, or, xor, complement
  , shiftLeftBy, shiftRightBy, shiftRightZfBy
  )


import Basics exposing (..)


and : Int -> Int -> Int
and =
  Elm.Kernel.Bitwise.and


or : Int -> Int -> Int
or =
  Elm.Kernel.Bitwise.or


xor : Int -> Int -> Int
xor =
  Elm.Kernel.Bitwise.xor


complement : Int -> Int
complement =
  Elm.Kernel.Bitwise.complement


shiftLeftBy : Int -> Int -> Int
shiftLeftBy =
  Elm.Kernel.Bitwise.shiftLeftBy


shiftRightBy : Int -> Int -> Int
shiftRightBy =
  Elm.Kernel.Bitwise.shiftRightBy


shiftRightZfBy : Int -> Int -> Int
shiftRightZfBy =
  Elm.Kernel.Bitwise.shiftRightZfBy
