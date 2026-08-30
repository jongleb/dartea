module Main exposing (Model, Msg(..), Route(..), main, routeOf)

import Browser
import Browser.Navigation as Navigation
import Html exposing (Html, a, div, h1, li, text, ul)
import Html.Attributes exposing (href)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, int, s, top)


type Route
    = Home
    | Post Int
    | Unknown


type alias Model =
    { key : Navigation.Key
    , route : Route
    }


type Msg
    = Clicked Browser.UrlRequest
    | Changed Url


route : Parser (Route -> a) a
route =
    Parser.oneOf
        [ Parser.map Home top
        , Parser.map Post (s "post" </> int)
        ]


routeOf : Url -> Route
routeOf url =
    Maybe.withDefault Unknown (Parser.parse route url)


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init () url key =
    ( { key = key, route = routeOf url }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clicked (Browser.Internal url) ->
            ( model, Navigation.pushUrl model.key (Url.toString url) )

        Clicked (Browser.External href) ->
            ( model, Navigation.load href )

        Changed url ->
            ( { model | route = routeOf url }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = title model.route
    , body =
        [ h1 [] [ text (title model.route) ]
        , ul []
            [ li [] [ a [ href "/" ] [ text "home" ] ]
            , li [] [ a [ href "/post/7" ] [ text "post 7" ] ]
            , li [] [ a [ href "https://elm-lang.org" ] [ text "elsewhere" ] ]
            ]
        ]
    }


title : Route -> String
title found =
    case found of
        Home ->
            "Home"

        Post id ->
            "Post " ++ String.fromInt id

        Unknown ->
            "Not found"


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = Clicked
        , onUrlChange = Changed
        }
