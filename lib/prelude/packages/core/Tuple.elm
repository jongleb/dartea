{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/core -- https://github.com/elm/core
   Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Tuple exposing
    ( pair
    , first, second
    )


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
