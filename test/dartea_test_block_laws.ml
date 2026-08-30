open QCheck2

type attribute = Bare | Static_class of string | Dynamic_class

type tree =
  | Element of { tag : string; attribute : attribute; children : tree list }
  | Static of string
  | Dynamic_text
  | Rows
  | Branch of tree * tree

let tags = [ "div"; "span"; "p"; "section"; "b" ]
let words = [ "ab"; "cd"; "ef"; "gh" ]

let gen_word = Gen.oneof_list words

let gen_attribute =
  Gen.oneof
    [ Gen.return Bare; Gen.map (fun word -> Static_class word) gen_word; Gen.return Dynamic_class ]

let gen_tree =
  Gen.sized
  @@ Gen.fix (fun self size ->
         let leaf =
           Gen.oneof
             [
               Gen.map (fun word -> Static word) gen_word;
               Gen.return Dynamic_text;
               Gen.return Rows;
             ]
         in
         if size <= 0 then leaf
         else
           Gen.oneof
             [
               leaf;
               Gen.map3
                 (fun tag attribute children -> Element { tag; attribute; children })
                 (Gen.oneof_list tags) gen_attribute
                 (Gen.list_size (Gen.int_range 0 3) (self (size / 2)));
               Gen.map2 (fun one other -> Branch (one, other)) (self (size / 2)) (self (size / 2));
             ])

let rec elm tree =
  match tree with
  | Element { tag; attribute; children } ->
      let attributes =
        match attribute with
        | Bare -> "[]"
        | Static_class word -> Printf.sprintf "[ class \"%s\" ]" word
        | Dynamic_class -> "[ class (if model > 0 then \"on\" else \"off\") ]"
      in
      Printf.sprintf "%s %s [ %s ]" tag attributes (String.concat ", " (List.map elm children))
  | Static word -> Printf.sprintf "text \"%s\"" word
  | Dynamic_text -> "text (String.fromInt model)"
  | Rows -> "ul [] (List.map (\\i -> li [] [ text (String.fromInt i) ]) (List.range 1 model))"
  | Branch (one, other) -> Printf.sprintf "(if model > 0 then %s else %s)" (elm one) (elm other)

let program tree =
  Printf.sprintf
    {|module Main exposing (main)

import Browser
import Html exposing (Html, button, div, li, section, span, p, b, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type Msg
    = Bump


view : Int -> Html Msg
view model =
    div [] [ button [ onClick Bump ] [], %s ]


main : Program () Int Msg
main =
    Browser.sandbox { init = 0, update = \_ model -> model + 1, view = view }
|}
    (elm tree)

let rec expect tree model =
  match tree with
  | Element { tag; attribute; children } ->
      let shown =
        match attribute with
        | Bare -> ""
        | Static_class word -> Printf.sprintf " class=\"%s\"" word
        | Dynamic_class -> Printf.sprintf " class=\"%s\"" (if model > 0 then "on" else "off")
      in
      Printf.sprintf "<%s%s>%s</%s>" tag shown
        (String.concat "" (List.map (fun child -> expect child model) children))
        tag
  | Static word -> word
  | Dynamic_text -> string_of_int model
  | Rows ->
      let rows = List.init model (fun index -> Printf.sprintf "<li>%d</li>" (index + 1)) in
      Printf.sprintf "<ul>%s</ul>" (String.concat "" rows)
  | Branch (one, other) -> expect (if model > 0 then one else other) model

let page tree model = Printf.sprintf "<body><div><button></button>%s</div></body>" (expect tree model)

let stub =
  Sample.dom
  ^ {|const serial = (node) =>
  node.tag === undefined
    ? node.text
    : "<" + node.tag + Object.keys(node.attributes).sort().map((key) => " " + key + "=\"" + node.attributes[key] + "\"").join("") + ">" + node.childNodes.map(serial).join("") + "</" + node.tag + ">";
const host = await mounted();
const before = serial(host);
fired(found(host, "button"), "click");
await new Promise((settle) => setTimeout(settle));
console.log(before + "\n" + serial(host));
|}

let law_blocks_render_like_the_oracle =
  Test.make ~name:"blocks render exactly the oracle, before and after an update" ~count:25
    ~print:(fun tree -> program tree)
    gen_tree
    (fun tree ->
      let expected = page tree 0 ^ "\n" ^ page tree 1 in
      let actual = Dartea_test_delivery.printed_by ~program:(program tree) ~stub in
      if String.equal expected actual then true
      else Test.fail_reportf "expected:\n%s\nbut got:\n%s" expected actual)

let suite = QCheck_ounit.to_ounit2_test_list [ law_blocks_render_like_the_oracle ]
