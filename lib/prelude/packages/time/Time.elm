{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/time -- https://github.com/elm/time
   Copyright 2018-present Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


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
