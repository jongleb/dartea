module Main exposing (Model, Msg(..), main)

import Browser
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


type alias Model =
    { greeting : String
    , answer : String
    , failure : String
    }


type Msg
    = Fetch
    | Got (Result Http.Error String)
    | Posted (Result Http.Error Int)


init : () -> ( Model, Cmd Msg )
init () =
    ( { greeting = "", answer = "", failure = "" }
    , Http.get { url = "/hello", expect = Http.expectString Got }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch ->
            ( model
            , Http.post
                { url = "/double"
                , body = Http.jsonBody (Encode.object [ ( "n", Encode.int 21 ) ])
                , expect = Http.expectJson Posted (Decode.field "doubled" Decode.int)
                }
            )

        Got (Ok greeting) ->
            ( { model | greeting = greeting }, Cmd.none )

        Got (Err problem) ->
            ( { model | failure = describe problem }, Cmd.none )

        Posted (Ok doubled) ->
            ( { model | answer = String.fromInt doubled }, Cmd.none )

        Posted (Err problem) ->
            ( { model | failure = describe problem }, Cmd.none )


describe : Http.Error -> String
describe problem =
    case problem of
        Http.BadUrl url ->
            "bad url " ++ url

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "network"

        Http.BadStatus code ->
            "status " ++ String.fromInt code

        Http.BadBody reason ->
            "body " ++ reason


view : Model -> Html Msg
view model =
    div []
        [ p [ id "greeting" ] [ text model.greeting ]
        , p [ id "answer" ] [ text model.answer ]
        , p [ id "failure" ] [ text model.failure ]
        , button [ onClick Fetch ] [ text "double" ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
