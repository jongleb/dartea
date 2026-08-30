module Time exposing (Posix, every, millisToPosix, posixToMillis)

import Basics exposing (..)
import Platform.Sub exposing (Sub)


type Posix
    = Posix Int


millisToPosix : Int -> Posix
millisToPosix =
    Posix


posixToMillis : Posix -> Int
posixToMillis stamp =
    case stamp of
        Posix millis ->
            millis


every : Float -> (Posix -> msg) -> Sub msg
every =
    Elm.Kernel.Time.every
