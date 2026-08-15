module Palette exposing (Slot, slot, brighter, distance)

import Color exposing (Color(..))


type alias Slot =
    Color


slot : Int -> Slot
slot n =
    if n == 0 then
        Red

    else if n == 1 then
        Green

    else
        Blue


brighter : Slot -> Slot
brighter color =
    case color of
        Red ->
            Green

        Green ->
            Blue

        Blue ->
            Red


distance : Slot -> Slot -> Int
distance from to =
    Color.toCode to - Color.toCode from
