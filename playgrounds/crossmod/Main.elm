module Main exposing (main, label)

import Color as C
import Palette exposing (brighter, distance, slot)


label : Int -> String
label n =
    C.name (slot n)


main : Int
main =
    distance (slot 0) (brighter (slot 1)) + C.toCode (slot 2)
