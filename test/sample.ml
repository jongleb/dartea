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

let dom_stub = {|let created = 0;
let replaced = 0;

const make = (tag) => {
  const node = {
    tag,
    listeners: {},
    attributes: {},
    childNodes: [],
    style: { setProperty() {}, removeProperty() {} },
    setAttribute(key, value) { this.attributes[key] = value; },
    removeAttribute(key) { delete this.attributes[key]; },
    addEventListener(event, handler) { this.listeners[event] = handler; },
    appendChild(child) { this.childNodes.push(child); return child; },
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
  };
  return node;
};

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

const clicked = (node) => {
  if (node.listeners && node.listeners.click) {
    node.listeners.click();
    return true;
  }
  for (const child of node.childNodes ?? []) if (clicked(child)) return true;
  return false;
};

const found = (node) => {
  if (node.listeners && node.listeners.click) return node;
  for (const child of node.childNodes ?? []) {
    const inside = found(child);
    if (inside) return inside;
  }
  return undefined;
};

const { mount } = await import("./main.js");
const host = make("body");
mount(host);
const first = shown(host);
const button = found(host);
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
    (found(host) === button) +
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

let typing_stub = {|let stopped = 0;
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
  removeChild(child) {
    this.childNodes = this.childNodes.filter((kept) => kept !== child);
  },
  replaceChild(next, previous) {
    this.childNodes = this.childNodes.map((kept) =>
      kept === previous ? next : kept,
    );
  },
  set textContent(_) { this.childNodes = []; },
  get textContent() { return ""; },
});

globalThis.document = {
  createElement: make,
  createTextNode: (text) => ({ text, childNodes: [] }),
};

const shown = (node) =>
  node.nodeValue !== undefined
    ? node.nodeValue
    : node.text !== undefined
      ? node.text
      : node.childNodes.map(shown).join("");

const found = (node, tag) => {
  if (node.tag === tag) return node;
  for (const child of node.childNodes ?? []) {
    const inside = found(child, tag);
    if (inside) return inside;
  }
  return undefined;
};

const happening = (extra) => ({
  ...extra,
  stopPropagation() { stopped += 1; },
  preventDefault() { prevented += 1; },
});

const { mount } = await import("./main.js");
const host = make("body");
mount(host);
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

let property_stub = {|const make = (tag) => ({
  tag,
  listeners: {},
  attributes: {},
  childNodes: [],
  style: { setProperty() {}, removeProperty() {} },
  setAttribute(key, value) { this.attributes[key] = value; },
  removeAttribute(key) { delete this.attributes[key]; },
  addEventListener(event, handler) { this.listeners[event] = handler; },
  appendChild(child) { this.childNodes.push(child); return child; },
  removeChild() {},
  replaceChild() {},
  set textContent(_) { this.childNodes = []; },
  get textContent() { return ""; },
});

globalThis.document = {
  createElement: make,
  createTextNode: (text) => ({ text, childNodes: [] }),
};

const found = (node, tag) => {
  if (node.tag === tag) return node;
  for (const child of node.childNodes ?? []) {
    const inside = found(child, tag);
    if (inside) return inside;
  }
  return undefined;
};

const { mount } = await import("./main.js");
const host = make("body");
mount(host);
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
