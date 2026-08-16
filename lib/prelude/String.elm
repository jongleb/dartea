module String exposing
    ( length, append
    , toInt, fromInt
    , toFloat, fromFloat
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


fromFloat : Float -> String
fromFloat =
    Elm.Kernel.String.fromNumber


toFloat : String -> Maybe Float
toFloat string =
    if Elm.Kernel.String.isFloat string then
        Just (Elm.Kernel.String.toFloatUnsafe string)

    else
        Nothing


toInt : String -> Maybe Int
toInt string =
    if Elm.Kernel.String.isInt string then
        Just (Elm.Kernel.String.toIntUnsafe string)

    else
        Nothing
