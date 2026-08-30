module Main exposing (main)

import Browser
import Html exposing (Html, a, button, div, h1, span, table, td, text, tr)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed


type alias Row =
    { id : Int
    , label : String
    }


type alias Model =
    { rows : List Row
    , selected : Int
    , next : Int
    , seed : Int
    }


type Msg
    = Create Int
    | Append Int
    | Update
    | Clear
    | Swap
    | Select Int
    | Remove Int


adjectives : List String
adjectives =
    [ "pretty", "large", "big", "small", "tall", "short", "long", "handsome", "plain", "quaint", "clean", "elegant", "easy", "angry", "crazy", "helpful", "mushy", "odd", "unsightly", "adorable", "important", "inexpensive", "cheap", "expensive", "fancy" ]


colours : List String
colours =
    [ "red", "yellow", "blue", "green", "pink", "brown", "purple", "brown", "white", "black", "orange" ]


nouns : List String
nouns =
    [ "table", "chair", "house", "bbq", "desk", "car", "pony", "cookie", "sandwich", "burger", "pizza", "mouse", "keyboard" ]


pick : Int -> List String -> String
pick seed words =
    List.drop (modBy (List.length words) seed) words
        |> List.head
        |> Maybe.withDefault ""


label : Int -> String
label seed =
    pick seed adjectives ++ " " ++ pick (seed // 7) colours ++ " " ++ pick (seed // 3) nouns


build : Int -> Model -> ( List Row, Model )
build count model =
    let
        ids =
            List.range model.next (model.next + count - 1)

        rows =
            List.indexedMap (\offset rowId -> { id = rowId, label = label (model.seed + offset * 13) }) ids
    in
    ( rows, { model | next = model.next + count, seed = model.seed + count * 13 } )


init : () -> ( Model, Cmd Msg )
init () =
    ( { rows = [], selected = 0, next = 1, seed = 0 }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Create count ->
            let
                ( rows, next ) =
                    build count model
            in
            ( { next | rows = rows }, Cmd.none )

        Append count ->
            let
                ( rows, next ) =
                    build count model
            in
            ( { next | rows = model.rows ++ rows }, Cmd.none )

        Update ->
            ( { model | rows = List.indexedMap bump model.rows }, Cmd.none )

        Clear ->
            ( { model | rows = [] }, Cmd.none )

        Swap ->
            ( { model | rows = swap model.rows }, Cmd.none )

        Select rowId ->
            ( { model | selected = rowId }, Cmd.none )

        Remove rowId ->
            ( { model | rows = List.filter (\row -> row.id /= rowId) model.rows }, Cmd.none )


bump : Int -> Row -> Row
bump index row =
    if modBy 10 index == 0 then
        { row | label = row.label ++ " !!!" }

    else
        row


swap : List Row -> List Row
swap rows =
    case rows of
        first :: second :: rest ->
            case List.reverse rest of
                last :: middle ->
                    first :: last :: List.reverse middle ++ [ second ]

                [] ->
                    rows

        _ ->
            rows


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "jumbotron" ]
            [ h1 [] [ text "dartea keyed" ]
            , button [ id "run", onClick (Create 1000) ] [ text "Create 1,000 rows" ]
            , button [ id "runlots", onClick (Create 10000) ] [ text "Create 10,000 rows" ]
            , button [ id "add", onClick (Append 1000) ] [ text "Append 1,000 rows" ]
            , button [ id "update", onClick Update ] [ text "Update every 10th row" ]
            , button [ id "clear", onClick Clear ] [ text "Clear" ]
            , button [ id "swaprows", onClick Swap ] [ text "Swap Rows" ]
            ]
        , table [ class "table" ]
            [ Keyed.node "tbody" [] (List.map (viewRow model.selected) model.rows) ]
        ]


viewRow : Int -> Row -> ( String, Html Msg )
viewRow selected row =
    ( String.fromInt row.id
    , tr
        [ class
            (if selected == row.id then
                "danger"

             else
                ""
            )
        ]
        [ td [ class "col-md-1" ] [ text (String.fromInt row.id) ]
        , td [ class "col-md-4" ] [ a [ onClick (Select row.id) ] [ text row.label ] ]
        , td [ class "col-md-1" ] [ button [ class "remove", onClick (Remove row.id) ] [ text "x" ] ]
        , td [ class "col-md-6" ] []
        ]
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
