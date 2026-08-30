module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, a, button, div, h1, span, table, td, text, tr)
import Html.Attributes exposing (attribute, class, classList, id, type_)
import Html.Events exposing (onClick)
import Html.Keyed


main : Program Int Model Msg
main =
    Browser.element
        { view = view
        , update = update
        , init = init
        , subscriptions = \_ -> Sub.none
        }


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


step : Int -> Int
step seed =
    modBy 2147483647 (seed * 48271)


label : Int -> String
label seed =
    pick seed adjectives ++ " " ++ pick (step seed) colours ++ " " ++ pick (step (step seed)) nouns


buttons : List ( String, String, Msg )
buttons =
    [ ( "run", "Create 1,000 rows", Create 1000 )
    , ( "runlots", "Create 10,000 rows", Create 10000 )
    , ( "add", "Append 1,000 rows", Append 1000 )
    , ( "update", "Update every 10th row", UpdateEveryTenth )
    , ( "clear", "Clear", Clear )
    , ( "swaprows", "Swap Rows", Swap )
    ]


viewButton : ( String, String, Msg ) -> Html Msg
viewButton ( buttonId, labelText, msg ) =
    div
        [ class "col-sm-6 smallpad" ]
        [ button
            [ type_ "button"
            , class "btn btn-primary btn-block"
            , id buttonId
            , onClick msg
            , attribute "ref" "text"
            ]
            [ text labelText ]
        ]


viewKeyedRow : Int -> Row -> ( String, Html Msg )
viewKeyedRow selectedId row =
    ( String.fromInt row.id, viewRow (selectedId == row.id) row )


viewRow : Bool -> Row -> Html Msg
viewRow isSelected { id, label } =
    tr
        [ classList [ ( "danger", isSelected ) ] ]
        [ td [ class "col-md-1" ] [ text (String.fromInt id) ]
        , td [ class "col-md-4" ] [ a [ onClick (Select id) ] [ text label ] ]
        , td [ class "col-md-1" ]
            [ a [ onClick (Remove id) ]
                [ span [ class "glyphicon glyphicon-remove", attribute "aria-hidden" "true" ] [] ]
            ]
        , td [ class "col-md-6" ] []
        ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "jumbotron" ]
            [ div [ class "row" ]
                [ div [ class "col-md-6" ] [ h1 [] [ text "dartea" ] ]
                , div [ class "col-md-6" ] (List.map viewButton buttons)
                ]
            ]
        , table [ class "table table-hover table-striped test-data" ]
            [ Html.Keyed.node "tbody" [] (List.map (viewKeyedRow model.selectedId) model.rows) ]
        , span [ class "preloadicon glyphicon glyphicon-remove", attribute "aria-hidden" "true" ] []
        ]


type alias Row =
    { id : Int
    , label : String
    }


type alias Model =
    { seed : Int
    , rows : List Row
    , lastId : Int
    , selectedId : Int
    }


type Msg
    = Create Int
    | Append Int
    | UpdateEveryTenth
    | Clear
    | Swap
    | Remove Int
    | Select Int


init : Int -> ( Model, Cmd Msg )
init systemTime =
    ( { seed = systemTime, rows = [], lastId = 0, selectedId = 0 }, Cmd.none )


build : Int -> Model -> ( List Row, Int )
build amount model =
    let
        ids =
            List.range (model.lastId + 1) (model.lastId + amount)

        seeds =
            List.foldl (\_ acc -> step (Maybe.withDefault model.seed (List.head acc)) :: acc) [ model.seed ] ids
                |> List.reverse
                |> List.drop 1

        rows =
            List.map2 (\rowId seed -> { id = rowId, label = label seed }) ids seeds
    in
    ( rows, Maybe.withDefault model.seed (List.head (List.reverse seeds)) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Create amount ->
            let
                ( rows, seed ) =
                    build amount model
            in
            ( { model | rows = rows, seed = seed, lastId = model.lastId + amount }, Cmd.none )

        Append amount ->
            let
                ( rows, seed ) =
                    build amount model
            in
            ( { model | rows = model.rows ++ rows, seed = seed, lastId = model.lastId + amount }, Cmd.none )

        UpdateEveryTenth ->
            ( { model | rows = List.indexedMap bump model.rows }, Cmd.none )

        Clear ->
            ( { model | rows = [] }, Cmd.none )

        Swap ->
            ( { model | rows = swap model.rows }, Cmd.none )

        Remove rowId ->
            ( { model | rows = List.filter (\row -> row.id /= rowId) model.rows }, Cmd.none )

        Select rowId ->
            ( { model | selectedId = rowId }, Cmd.none )


bump : Int -> Row -> Row
bump index row =
    if modBy 10 index == 0 then
        { row | label = row.label ++ " !!!" }

    else
        row


swap : List Row -> List Row
swap rows =
    let
        at index =
            List.head (List.drop index rows)
    in
    case ( at 1, at 998 ) of
        ( Just second, Just target ) ->
            List.indexedMap
                (\index row ->
                    if index == 1 then
                        target

                    else if index == 998 then
                        second

                    else
                        row
                )
                rows

        _ ->
            rows
