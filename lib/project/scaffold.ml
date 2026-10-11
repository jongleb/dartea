let create root =
  let manifest =
    {|{
    "source-directories": [
        "src"
    ],
    "dependencies": {}
}
|}
  in
  let main =
    {|module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type Msg
    = Increment
    | Decrement


update : Msg -> Int -> Int
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view : Int -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , text (String.fromInt model)
        , button [ onClick Increment ] [ text "+" ]
        ]


main : Program () Int Msg
main =
    Browser.sandbox { init = 0, update = update, view = view }
|}
  in
  let files = [ (Packages.Manifest.path, manifest); (Fpath.v "src/Main.elm", main) ] in
  Option.iter
    (fun path ->
      Diagnostic.Failure.raise_project
        (Existing_project { file = Fpath.to_string path }))
    (List.find_opt (Files.is_file root) [ Packages.Manifest.path; Outline.path ]);
  List.iter (fun (path, content) -> Files.save root path content) files;
  List.map fst files
