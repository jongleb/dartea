{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/core -- https://github.com/elm/core
   Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Tuple exposing
    ( first, mapBoth, mapFirst, mapSecond, pair, second )


pair : a -> b -> ( a, b )
pair a b =
    ( a, b )


first : ( a1, a2 ) -> a1
first t =
    case t of
        ( x, y ) ->
            x


second : ( a1, a2 ) -> a2
second t =
    case t of
        ( x, y ) ->
            y


mapFirst : (a -> x) -> (a, b) -> (x, b)
mapFirst func (x,y) =
  (func x, y)



mapSecond : (b -> y) -> (a, b) -> (a, y)
mapSecond func (x,y) =
  (x, func y)



mapBoth : (a -> x) -> (b -> y) -> (a, b) -> (x, y)
mapBoth funcA funcB (x,y) =
  ( funcA x, funcB y )
