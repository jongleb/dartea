module Color exposing (Color(..), toCode, name)


type Color
    = Red
    | Green
    | Blue


toCode : Color -> Int
toCode color =
    case color of
        Red ->
            1

        Green ->
            2

        Blue ->
            3


name : Color -> String
name color =
    case color of
        Red ->
            "red"

        Green ->
            "green"

        Blue ->
            "blue"


internalNote : String
internalNote =
    "Color keeps this to itself"
