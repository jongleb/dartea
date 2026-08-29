open OUnit2

let playground_root = Filename.concat ".." "playgrounds"

let playgrounds =
  Sys.readdir playground_root |> Array.to_list
  |> List.filter (fun entry ->
         let path = Filename.concat playground_root entry in
         Sys.is_directory path
         && Array.exists
              (fun file ->
                Filename.check_suffix file Project.Elm_file.extension)
              (Sys.readdir path))
  |> List.sort String.compare

let refused sources errors =
  assert_failure
    (String.concat "\n"
       (List.map
          (fun error ->
            Reporting.Report.to_string ~colours:false
              (Reporting.Sources.report sources error))
          errors))

let shaken ~roots (outcome : Dartea.Compiler.outcome) =
  match outcome.errors with
  | [] ->
      List.map
        (fun (compiled : Dartea.Compiler.compiled) ->
          (compiled.module_name, compiled.source))
        (Dartea.Compiler.link ~roots outcome)
  | errors -> refused (Reporting.Sources.of_list outcome.sources) errors

let emitted outcome = shaken ~roots:(Dartea.Compiler.everything outcome) outcome

let delivered ~delivery outcome =
  let module Delivery = (val delivery : Dartea.Delivery.S) in
  shaken ~roots:(Delivery.roots outcome) outcome

let compiled_in folder =
  Eio_main.run @@ fun env ->
  match Project.Sources.load Eio.Path.(Eio.Stdenv.fs env / folder) with
  | Ok sources -> Dartea.Compiler.compile_modules ~entry:None sources
  | Error error -> refused Reporting.Sources.empty [ error ]

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
let styled = 0;
let attributed = 0;
let erased = 0;
let written = 0;
let stopped = 0;
let prevented = 0;

const make = (tag) => ({
  tag,
  listeners: {},
  attributes: {},
  namespaces: {},
  childNodes: [],
  style: { setProperty() { styled += 1; }, removeProperty() {} },
  setAttribute(key, value) { attributed += 1; this.attributes[key] = value; },
  setAttributeNS(namespace, key, value) {
    this.attributes[key] = value;
    this.namespaces[key] = namespace;
  },
  removeAttributeNS(namespace, key) { delete this.attributes[key]; },
  removeAttribute(key) { erased += 1; delete this.attributes[key]; },
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
  createElementNS: (namespace, tag) => {
    created += 1;
    const node = make(tag);
    node.namespace = namespace;
    return node;
  },
  createTextNode: (text) => {
    let held = text;
    return {
      childNodes: [],
      get text() { return held; },
      get nodeValue() { return held; },
      set nodeValue(next) { written += 1; held = next; },
    };
  },
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

let counted_program = {|module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import VirtualDom


type Msg
    = Bumped
    | Painted
    | Stripped


type alias Model =
    { count : Int, colour : String, marked : Bool }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Bumped ->
            { model | count = model.count + 1 }

        Painted ->
            { model | colour = "green" }

        Stripped ->
            { model | marked = False }


mark : Model -> List (Html.Attribute Msg)
mark model =
    if model.marked then
        [ VirtualDom.attribute "data-mark" "yes" ]

    else
        []


nest : Int -> Model -> Html Msg
nest depth model =
    if depth == 0 then
        div
            (VirtualDom.attribute "data-level" "leaf"
                :: style "color" model.colour
                :: mark model
            )
            [ text (String.fromInt model.count) ]

    else
        div
            [ VirtualDom.attribute "data-level" (String.fromInt depth)
            , style "color" "blue"
            ]
            [ nest (depth - 1) model ]


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Bumped ] [ text "b" ]
        , button [ onClick Painted ] [ text "p" ]
        , button [ onClick Stripped ] [ text "s" ]
        , nest 6 model
        ]


main : Browser.Program () Model Msg
main =
    Browser.sandbox
        { init = { count = 0, colour = "red", marked = True }
        , update = update
        , view = view
        }
|}

let counted_stub = dom ^ {|const host = await mounted();
const [bump, paint, strip] = tagged(host, "button");
const counted = (which) => {
  attributed = 0;
  styled = 0;
  erased = 0;
  written = 0;
  which.listeners.click(happening({}));
  return [attributed, styled, erased, written].join("/");
};
console.log(
  "bump " +
    counted(bump) +
    " | paint " +
    counted(paint) +
    " | strip " +
    counted(strip) +
    " | " +
    JSON.stringify(shown(host)),
);
|}

let guarded_program = {|module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (href)
import Json.Encode
import VirtualDom


type Msg
    = Ignored


update : Msg -> Int -> Int
update _ model =
    model


link : String -> Html Msg
link written =
    VirtualDom.node "a" [ href written ] []


view : Int -> Html Msg
view _ =
    VirtualDom.node "div"
        []
        [ VirtualDom.node "script" [] []
        , VirtualDom.nodeNS "http://www.w3.org/2000/svg"
            "svg"
            [ VirtualDom.attributeNS "http://www.w3.org/1999/xlink"
                "xlink:href"
                "#icon"
            , VirtualDom.attribute "viewBox" "0 0 10 10"
            ]
            []
        , link "javascript:alert(1)"
        , link "   JaVaScRiPt:alert(2)"
        , link "java\tscript:alert(3)"
        , link "data:text/html,<b>"
        , link "/safe"
        , VirtualDom.node "b" [ VirtualDom.attribute "onclick" "boom()" ] []
        , VirtualDom.node "i"
            [ VirtualDom.property "innerHTML" (Json.Encode.string "<b>") ]
            []
        ]


main : Browser.Program () Int Msg
main =
    Browser.sandbox { init = 0, update = update, view = view }
|}

let guarded_stub = dom ^ {|const host = await mounted();
const kids = host.childNodes[0].childNodes;
const svg = kids[1];
console.log(
  [
    "script " + kids[0].tag,
    "svg " + svg.namespace,
    "xlink " + svg.namespaces["xlink:href"] + " " + svg.attributes["xlink:href"],
    "viewBox " + svg.namespaces.viewBox,
    "hrefs " + JSON.stringify(kids.slice(2, 7).map((node) => node.href)),
    "onclick " + JSON.stringify(kids[7].attributes),
    "innerHTML " + JSON.stringify(kids[8]["data-innerHTML"]),
  ].join(" | "),
);
|}

let lazy_program = {|module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.Lazy


type Msg
    = Bumped
    | Renamed


type alias Model =
    { count : Int, label : String }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Bumped ->
            { model | count = model.count + 1 }

        Renamed ->
            { model | label = model.label ++ "!" }


tile : String -> Html Msg
tile written =
    div [ style "color" written ] [ text written ]


v1 : String -> Html Msg
v1 a =
    tile (a)


v2 : String -> String -> Html Msg
v2 a b =
    tile (a ++ b)


v3 : String -> String -> String -> Html Msg
v3 a b c =
    tile (a ++ b ++ c)


v4 : String -> String -> String -> String -> Html Msg
v4 a b c d =
    tile (a ++ b ++ c ++ d)


v5 : String -> String -> String -> String -> String -> Html Msg
v5 a b c d e =
    tile (a ++ b ++ c ++ d ++ e)


v6 : String -> String -> String -> String -> String -> String -> Html Msg
v6 a b c d e f =
    tile (a ++ b ++ c ++ d ++ e ++ f)


v7 : String -> String -> String -> String -> String -> String -> String -> Html Msg
v7 a b c d e f g =
    tile (a ++ b ++ c ++ d ++ e ++ f ++ g)


v8 : String -> String -> String -> String -> String -> String -> String -> String -> Html Msg
v8 a b c d e f g h =
    tile (a ++ b ++ c ++ d ++ e ++ f ++ g ++ h)


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Bumped ] [ text "b" ]
        , button [ onClick Renamed ] [ text "r" ]
        , div [] [ text (String.fromInt model.count) ]
        , Html.Lazy.lazy v1 model.label
        , Html.Lazy.lazy2 v2 model.label "2"
        , Html.Lazy.lazy3 v3 model.label "2" "3"
        , Html.Lazy.lazy4 v4 model.label "2" "3" "4"
        , Html.Lazy.lazy5 v5 model.label "2" "3" "4" "5"
        , Html.Lazy.lazy6 v6 model.label "2" "3" "4" "5" "6"
        , Html.Lazy.lazy7 v7 model.label "2" "3" "4" "5" "6" "7"
        , Html.Lazy.lazy8 v8 model.label "2" "3" "4" "5" "6" "7" "8"
        ]


main : Browser.Program () Model Msg
main =
    Browser.sandbox
        { init = { count = 0, label = "-" }, update = update, view = view }
|}

let lazy_stub = dom ^ {|const host = await mounted();
const [bump, rename] = tagged(host, "button");
styled = 0;
bump.listeners.click(happening({}));
const quiet = styled;
styled = 0;
rename.listeners.click(happening({}));
console.log(
  shown(host) +
    " (skipped " +
    quiet +
    ", recomputed " +
    styled +
    ")",
);
|}

let reshaped_program = {|module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type Inner
    = Poked


type Msg
    = Flipped
    | Wrapped Inner


type alias Model =
    { flipped : Bool, log : String }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Flipped ->
            { model | flipped = not model.flipped }

        Wrapped Poked ->
            { model | log = model.log ++ "P" }


inner : Bool -> Html Inner
inner flipped =
    if flipped then
        div [ onClick Poked ] [ text "block" ]

    else
        text "flat"


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Flipped ] [ text "flip" ]
        , Html.map Wrapped (inner model.flipped)
        , div [] [ text ("[" ++ model.log ++ "]") ]
        ]


main : Browser.Program () Model Msg
main =
    Browser.sandbox
        { init = { flipped = False, log = "" }, update = update, view = view }
|}

let reshaped_stub = dom ^ {|const host = await mounted();
const flip = tagged(host, "button")[0];
const listening = () =>
  tagged(host, "div").find((node) => node.listeners.click !== undefined);
flip.listeners.click(happening({}));
listening().listeners.click(happening({}));
flip.listeners.click(happening({}));
flip.listeners.click(happening({}));
listening().listeners.click(happening({}));
console.log(shown(host));
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
