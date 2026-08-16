module Tuple exposing
    ( pair
    , first, second
    )


pair : a -> b -> ( a, b )
pair =
    Elm.Kernel.Tuple.pair


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
