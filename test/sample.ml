let folder () = Filename.temp_dir "dartea" ""
let read path = In_channel.with_open_bin path In_channel.input_all

let rec ensured directory =
  if not (Sys.file_exists directory) then begin
    ensured (Filename.dirname directory);
    Sys.mkdir directory 0o755
  end

let written ~folder ~path content =
  let file = Filename.concat folder path in
  ensured (Filename.dirname file);
  Out_channel.with_open_bin file (fun out ->
      Out_channel.output_string out content)

let names spelled = String.concat ", " spelled
let sorted spelled = List.sort String.compare spelled

let rendered seen (error : Reporting.Error.t) =
  Reporting.Report.to_string ~colours:false
    (Reporting.Sources.report seen error)

let starter = {|module Main exposing (main)


main : String
main =
    String.fromInt (2 + 3) ++ " from dartea"
|}

let program = {|module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type Msg
    = Bumped


update : Msg -> Int -> Int
update msg model =
    case msg of
        Bumped ->
            model + 1


view : Int -> Html Msg
view model =
    div []
        [ button [ onClick Bumped ] [ text "+" ]
        , text (String.fromInt model)
        ]


main : Browser.Program () Int Msg
main =
    Browser.sandbox { init = 0, update = update, view = view }
|}

let dom_stub = {|const make = (tag) => ({
  tag, listeners: {}, children: [],
  style: { setProperty() {} },
  setAttribute() {},
  addEventListener(event, handler) { this.listeners[event] = handler; },
  appendChild(child) { this.children.push(child); return child; },
  set textContent(_) { this.children = []; },
  get textContent() { return ""; },
});
globalThis.document = {
  createElement: make,
  createTextNode: (text) => ({ text, children: [] }),
};
const shown = (node) =>
  node.text !== undefined ? node.text : node.children.map(shown).join("");
const clicked = (node) => {
  if (node.listeners && node.listeners.click) {
    node.listeners.click();
    return true;
  }
  for (const child of node.children ?? []) if (clicked(child)) return true;
  return false;
};
const { mount } = await import("./main.js");
const host = make("body");
mount(host);
const first = shown(host);
clicked(host);
clicked(host);
console.log(first + " -> " + shown(host));
|}
