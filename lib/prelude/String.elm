module String exposing
    ( length, append
    , toInt, fromInt
    )

import Maybe exposing (Maybe(..))


length : String -> Int
length =
    Elm.Kernel.String.length


append : String -> String -> String
append =
    Elm.Kernel.String.append


fromInt : Int -> String
fromInt =
    Elm.Kernel.String.fromNumber


toInt : String -> Maybe Int
toInt string =
    if Elm.Kernel.String.isInt string then
        Just (Elm.Kernel.String.toIntUnsafe string)

    else
        Nothing
