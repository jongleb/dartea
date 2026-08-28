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

let dom = {|let created = 0;
let replaced = 0;
let stopped = 0;
let prevented = 0;

const make = (tag) => ({
  tag,
  listeners: {},
  attributes: {},
  childNodes: [],
  style: { setProperty() {}, removeProperty() {} },
  setAttribute(key, value) { this.attributes[key] = value; },
  removeAttribute(key) { delete this.attributes[key]; },
  addEventListener(event, handler) { this.listeners[event] = handler; },
  appendChild(child) { this.childNodes.push(child); return child; },
  insertBefore(child, before) {
    this.childNodes = this.childNodes.filter((kept) => kept !== child);
    const at =
      before === null ? this.childNodes.length : this.childNodes.indexOf(before);
    this.childNodes.splice(at, 0, child);
    return child;
  },
  removeChild(child) {
    this.childNodes = this.childNodes.filter((kept) => kept !== child);
  },
  replaceChild(next, previous) {
    replaced += 1;
    this.childNodes = this.childNodes.map((kept) =>
      kept === previous ? next : kept,
    );
  },
  set textContent(_) { this.childNodes = []; },
  get textContent() { return ""; },
});

globalThis.document = {
  createElement: (tag) => { created += 1; return make(tag); },
  createTextNode: (text) => ({ text, childNodes: [] }),
};

const shown = (node) =>
  node.nodeValue !== undefined
    ? node.nodeValue
    : node.text !== undefined
      ? node.text
      : node.childNodes.map(shown).join("");

const tagged = (node, tag, into = []) => {
  if (node.tag === tag) into.push(node);
  for (const child of node.childNodes ?? []) tagged(child, tag, into);
  return into;
};

const found = (node, tag) => tagged(node, tag)[0];

const happening = (extra) => ({
  ...extra,
  stopPropagation() { stopped += 1; },
  preventDefault() { prevented += 1; },
});

const mounted = async () => {
  const { mount } = await import("./main.js");
  const host = make("body");
  mount(host);
  return host;
};
|}

let dom_stub = dom ^ {|const clicked = (node) => {
  if (node.listeners && node.listeners.click) {
    node.listeners.click(happening({}));
    return true;
  }
  for (const child of node.childNodes ?? []) if (clicked(child)) return true;
  return false;
};

const listening = (node) => {
  if (node.listeners && node.listeners.click) return node;
  for (const child of node.childNodes ?? []) {
    const inside = listening(child);
    if (inside) return inside;
  }
  return undefined;
};

const host = await mounted();
const first = shown(host);
const button = listening(host);
created = 0;
replaced = 0;
clicked(host);
clicked(host);
console.log(
  first +
    " -> " +
    shown(host) +
    " (created " +
    created +
    ", replaced " +
    replaced +
    ", same node " +
    (listening(host) === button) +
    ")",
);
|}

let typing_program = {|module Main exposing (main)

import Browser
import Html exposing (Html, div, form, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput, onSubmit)


type Msg
    = Typed String
    | Sent


update : Msg -> String -> String
update msg model =
    case msg of
        Typed given ->
            given

        Sent ->
            model ++ "!"


view : String -> Html Msg
view model =
    form [ onSubmit Sent ]
        [ input [ value model, onInput Typed ] []
        , div [] [ text ("<" ++ model ++ ">") ]
        ]


main : Browser.Program () String Msg
main =
    Browser.sandbox { init = "", update = update, view = view }
|}

let typing_stub = dom ^ {|const host = await mounted();
found(host, "input").listeners.input(happening({ target: { value: "ok" } }));
const typed = shown(found(host, "div"));
found(host, "form").listeners.submit(happening({}));
console.log(
  typed +
    " -> " +
    shown(found(host, "div")) +
    " (stopped " +
    stopped +
    ", prevented " +
    prevented +
    ", value " +
    JSON.stringify(found(host, "input").value) +
    ")",
);
|}

let mapped_program = {|module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)


type Inner
    = Poked


type Middle
    = Wrapped Inner


type Deep
    = Deep Inner


type Msg
    = Left Int Middle
    | Right Inner
    | Third Deep


poker : Html Inner
poker =
    button [ onClick Poked ] [ text "poke" ]


mapped : Html Deep
mapped =
    button [ Html.Attributes.map Deep (onClick Poked) ] [ text "attr" ]


update : Msg -> String -> String
update msg model =
    case msg of
        Left depth (Wrapped Poked) ->
            model ++ "L" ++ String.fromInt depth

        Right Poked ->
            model ++ "R"

        Third (Deep Poked) ->
            model ++ "A"


view : String -> Html Msg
view model =
    div []
        [ Html.map (Left (String.length model)) (Html.map Wrapped poker)
        , Html.map Right poker
        , Html.map Third mapped
        , div [] [ text ("[" ++ model ++ "]") ]
        ]


main : Browser.Program () String Msg
main =
    Browser.sandbox { init = "", update = update, view = view }
|}

let mapped_stub = dom ^ {|const host = await mounted();
const [left, right, third] = tagged(host, "button");
created = 0;
for (const which of [left, right, third, left]) which.listeners.click(happening({}));
console.log(
  shown(host),
  "(created",
  created,
  ", same buttons",
  tagged(host, "button")[0] === left,
  ")",
);
|}

let field_program = {|module Main exposing (main)

import Browser
import Html exposing (Html, div, input)
import Html.Attributes exposing (class, type_, value)


type Msg
    = Bumped


update : Msg -> Int -> Int
update _ model =
    model + 1


view : Int -> Html Msg
view model =
    div [ class "wrap" ]
        [ input [ type_ "text", value (String.fromInt model) ] [] ]


main : Browser.Program () Int Msg
main =
    Browser.sandbox { init = 3, update = update, view = view }
|}

let property_stub = dom ^ {|const host = await mounted();
const field = found(host, "input");
console.log(
  "property " +
    JSON.stringify(field.value) +
    ", attribute " +
    JSON.stringify(field.attributes.value) +
    ", class " +
    JSON.stringify(found(host, "div").className),
);
|}

let keyed_program = {|module Main exposing (main)

import Browser
import Html exposing (Html, button, div, li, text)
import Html.Events exposing (onClick)
import Html.Keyed


type Msg
    = Prepended
    | Reversed


update : Msg -> List Int -> List Int
update msg model =
    case msg of
        Prepended ->
            (0 - List.length model) :: model

        Reversed ->
            List.reverse model


row : Int -> ( String, Html Msg )
row n =
    ( String.fromInt n, li [] [ text (String.fromInt n) ] )


view : List Int -> Html Msg
view model =
    div []
        [ button [ onClick Prepended ] [ text "add" ]
        , button [ onClick Reversed ] [ text "flip" ]
        , Html.Keyed.ul [] (List.map row model)
        ]


main : Browser.Program () (List Int) Msg
main =
    Browser.sandbox { init = List.range 1 10, update = update, view = view }
|}

let keyed_stub = dom ^ {|const host = await mounted();
const list = found(host, "ul");
const kept = list.childNodes[0];
const rows = () => list.childNodes.map(shown).join(",");
const [prepend, flip] = tagged(host, "button");
created = 0;
prepend.listeners.click(happening({}));
const inserted = rows() + " (created " + created + ", kept " + (list.childNodes[1] === kept) + ")";
created = 0;
flip.listeners.click(happening({}));
console.log(
  inserted +
    " -> " +
    rows() +
    " (created " +
    created +
    ", kept " +
    (list.childNodes[9] === kept) +
    ")",
);
|}
