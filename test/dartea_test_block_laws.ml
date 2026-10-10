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
const once = serial(host);
fired(found(host, "button"), "click");
await new Promise((settle) => setTimeout(settle));
console.log(before + "\n" + once + "\n" + serial(host));
|}

let law_blocks_render_like_the_oracle =
  Test.make ~name:"blocks render exactly the oracle, before and after an update" ~count:40
    ~print:(fun tree -> program tree)
    gen_tree
    (fun tree ->
      let expected = page tree 0 ^ "\n" ^ page tree 1 ^ "\n" ^ page tree 2 in
      let actual = Dartea_test_delivery.printed_by ~program:(program tree) ~stub in
      if String.equal expected actual then true
      else Test.fail_reportf "expected:\n%s\nbut got:\n%s" expected actual)

type edit = Swap of int * int | Remove of int | Insert of int | Reverse | Fresh of int list | Rotate

let gen_ids = Gen.(map (List.sort_uniq Int.compare) (list_size (int_range 0 12) (int_range 0 30)) >>= shuffle_list)

let gen_edit =
  Gen.oneof
    [
      Gen.map2 (fun one other -> Swap (one, other)) Gen.nat_small Gen.nat_small;
      Gen.map (fun at -> Remove at) Gen.nat_small;
      Gen.map (fun at -> Insert at) Gen.nat_small;
      Gen.return Reverse;
      Gen.map (fun ids -> Fresh ids) gen_ids;
      Gen.return Rotate;
    ]

let applied ids edit =
  let size = List.length ids in
  match edit with
  | Swap (one, other) when size > 0 ->
      let one = one mod size and other = other mod size in
      List.mapi
        (fun at id ->
          if at = one then List.nth ids other else if at = other then List.nth ids one else id)
        ids
  | Remove at when size > 0 -> List.filteri (fun index _ -> index <> at mod size) ids
  | Insert at ->
      let fresh = 1 + List.fold_left max 30 ids in
      let at = at mod (size + 1) in
      List.filteri (fun index _ -> index < at) ids @ (fresh :: List.filteri (fun index _ -> index >= at) ids)
  | Reverse -> List.rev ids
  | Fresh ids -> ids
  | Rotate when size > 0 -> List.tl ids @ [ List.hd ids ]
  | Swap _ | Remove _ | Rotate -> ids

let gen_states =
  Gen.(
    map3
      (fun first one other ->
        let second = applied first one in
        [ first; second; applied second other ])
      gen_ids gen_edit gen_edit)

let keyed_program states =
  let shown ids = "[ " ^ String.concat ", " (List.map string_of_int ids) ^ " ]" in
  Printf.sprintf
    {|module Main exposing (main)

import Browser
import Html exposing (Html, button, div, li, text)
import Html.Events exposing (onClick)
import Html.Keyed


type Msg
    = Bump


row : Int -> ( String, Html Msg )
row id =
    ( String.fromInt id, li [] [ text (String.fromInt id) ] )


states : Int -> List Int
states model =
    case model of
        0 ->
            %s

        1 ->
            %s

        _ ->
            %s


view : Int -> Html Msg
view model =
    div [] [ button [ onClick Bump ] [], Html.Keyed.node "ul" [] (List.map row (states model)) ]


main : Program () Int Msg
main =
    Browser.sandbox { init = 0, update = \_ model -> model + 1, view = view }
|}
    (shown (List.nth states 0)) (shown (List.nth states 1)) (shown (List.nth states 2))

let law_keyed_rows_follow_every_edit =
  Test.make ~name:"keyed rows render exactly the oracle through swaps, removals, inserts and replacements"
    ~count:60 ~print:keyed_program gen_states (fun states ->
      let page ids =
        Printf.sprintf "<body><div><button></button><ul>%s</ul></div></body>"
          (String.concat "" (List.map (fun id -> Printf.sprintf "<li>%d</li>" id) ids))
      in
      let expected = String.concat "\n" (List.map page states) in
      let actual = Dartea_test_delivery.printed_by ~program:(keyed_program states) ~stub in
      if String.equal expected actual then true
      else Test.fail_reportf "expected:\n%s\nbut got:\n%s" expected actual)

let suite =
  QCheck_ounit.to_ounit2_test_list [ law_blocks_render_like_the_oracle; law_keyed_rows_follow_every_edit ]
