module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type Msg
    = Bumped
    | Reset


update : Msg -> Int -> Int
update msg model =
    case msg of
        Bumped ->
            model + 1

        Reset ->
            0


view : Int -> Html Msg
view model =
    div [ class "counter" ]
        [ button [ onClick Bumped ] [ text "+" ]
        , text (" " ++ String.fromInt model ++ " ")
        , button [ onClick Reset ] [ text "reset" ]
        ]


main : Browser.Program () Int Msg
main =
    Browser.sandbox { init = 0, update = update, view = view }
