open OUnit2

let source = Node_runner.source

let node_eval ~modules ~expr =
  let outcome =
    Dartea.Compiler.compile_modules ~entry:None
      (Project.Sources.of_list modules)
  in
  Node_runner.evaluate ~compiled:(Node_runner.output_of outcome) ~expr

let assert_runs ~modules ~expr ~expected =
  assert_equal ~printer:Fun.id expected (node_eval ~modules ~expr)

let color =
  source "Color.elm"
    {|
module Color exposing (Color(..), toCode, name)

type Color = Red | Green | Blue

toCode : Color -> Int
toCode color =
    case color of
        Red -> 1
        Green -> 2
        Blue -> 3

name : Color -> String
name color =
    case color of
        Red -> "red"
        Green -> "green"
        Blue -> "blue"

internalNote = "private"
|}

let palette =
  source "Palette.elm"
    {|
module Palette exposing (Slot, slot, brighter, distance)

import Color exposing (Color(..))

type alias Slot = Color

slot : Int -> Slot
slot n =
    if n == 0 then Red else if n == 1 then Green else Blue

brighter : Slot -> Slot
brighter color =
    case color of
        Red -> Green
        Green -> Blue
        Blue -> Red

distance : Slot -> Slot -> Int
distance from to =
    Color.toCode to - Color.toCode from
|}

let test_qualified_exposed_and_aliased_imports _ =
  let main =
    source "Main.elm"
      {|
module Main exposing (main)

import Color as C
import Palette exposing (brighter, distance, slot)

main : Int
main =
    distance (slot 0) (brighter (slot 1)) + C.toCode (slot 2)
|}
  in
  assert_runs ~modules:[ color; palette; main ] ~expr:"Main.main" ~expected:"5"

let test_imported_ctor_string_survives_the_boundary _ =
  let main =
    source "Main.elm"
      {|
module Main exposing (main)

import Color as C
import Palette exposing (slot)

main : String
main =
    C.name (slot 1)
|}
  in
  assert_runs ~modules:[ color; palette; main ] ~expr:"Main.main"
    ~expected:{|"green"|}

let test_pattern_match_on_imported_ctor_with_payload _ =
  let boxes =
    source "Boxes.elm"
      {|
module Boxes exposing (Box(..), unbox)

type Box = Box Int

unbox : Box -> Int
unbox box =
    case box of
        Box n -> n
|}
  in
  let main =
    source "Main.elm"
      {|
module Main exposing (main)

import Boxes exposing (Box(..), unbox)

main : Int
main =
    unbox (Box 41) + 1
|}
  in
  assert_runs ~modules:[ boxes; main ] ~expr:"Main.main" ~expected:"42"

let test_runtime_module_is_shared _ =
  let labels =
    source "Labels.elm"
      {|
module Labels exposing (label)

label : Int -> String
label n =
    "n=" ++ String.fromInt n
|}
  in
  let main =
    source "Main.elm"
      {|
module Main exposing (main)

import Labels exposing (label)

main : String
main =
    label 7
|}
  in
  assert_runs ~modules:[ labels; main ] ~expr:"Main.main" ~expected:{|"n=7"|}

let test_diamond_imports _ =
  let shared =
    source "Shared.elm" {|
module Shared exposing (base)

base : Int
base = 10
|}
  in
  let left =
    source "Left.elm"
      {|
module Left exposing (twice)

import Shared exposing (base)

twice : Int
twice = base * 2
|}
  in
  let right =
    source "Right.elm"
      {|
module Right exposing (thrice)

import Shared exposing (base)

thrice : Int
thrice = base * 3
|}
  in
  let main =
    source "Main.elm"
      {|
module Main exposing (main)

import Left exposing (twice)
import Right exposing (thrice)

main : Int
main = twice + thrice
|}
  in
  assert_runs ~modules:[ shared; left; right; main ] ~expr:"Main.main"
    ~expected:"50"

let test_imported_arity_crosses_the_boundary _ =
  let arith =
    source "Arith.elm"
      {|
module Arith exposing (add, applyTwo, adder, curried)

add : Int -> Int -> Int
add a b = a + b

adder : Int -> Int -> Int
adder n =
    let
        doubled = n + n
    in
    \m -> doubled + m

curried : Int -> Int -> Int -> Int
curried a =
    \b -> add (a + b)

applyTwo : (Int -> Int -> Int) -> Int
applyTwo f =
    case f 3 4 of
        0 ->
            100

        n ->
            n + f 1 1
|}
  in
  let main =
    source "Main.elm"
      {|
module Main exposing (viaAdd, viaAdder, viaIdentity, viaCurried)

import Arith

viaAdd : Int
viaAdd = Arith.applyTwo Arith.add

viaAdder : Int
viaAdder = Arith.applyTwo Arith.adder

viaIdentity : Int
viaIdentity = identity Arith.add 3 4

viaCurried : Int
viaCurried = Arith.curried 1 2 3
|}
  in
  assert_runs ~modules:[ arith; main ]
    ~expr:
      "[Main.viaAdd, Main.viaAdder, Main.viaIdentity, Main.viaCurried].join(\",\")"
    ~expected:{|"9,13,7,6"|}

let test_qualified_constructor_in_a_pattern _ =
  let main =
    source "Main.elm"
      {|
module Main exposing (viaQualified, viaQualifiedPayload)

import Color

describe : Color.Color -> Int
describe c =
    case c of
        Color.Red ->
            1

        Color.Green ->
            2

        Color.Blue ->
            3

viaQualified : Int
viaQualified = describe Color.Green

viaQualifiedPayload : Int
viaQualifiedPayload =
    case Color.Blue of
        Color.Blue ->
            30

        _ ->
            0
|}
  in
  assert_runs ~modules:[ color; main ]
    ~expr:"[Main.viaQualified, Main.viaQualifiedPayload].join(\",\")"
    ~expected:{|"2,30"|}

let test_record_alias_constructor_crosses_the_boundary _ =
  let shapes =
    source "Shapes.elm"
      {|
module Shapes exposing (Point, origin)

type alias Point =
    { x : Int
    , y : Int
    }

origin : Point
origin = Point 0 0
|}
  in
  let main =
    source "Main.elm"
      {|
module Main exposing (viaQualifiedAlias, viaExposedAlias)

import Shapes exposing (Point)

made : Point
made = Point 3 4

viaExposedAlias : Int
viaExposedAlias = made.x + made.y

viaQualifiedAlias : Int
viaQualifiedAlias = (Shapes.Point 5 6).x + Shapes.origin.y
|}
  in
  assert_runs ~modules:[ shapes; main ]
    ~expr:"[Main.viaExposedAlias, Main.viaQualifiedAlias].join(\",\")"
    ~expected:{|"7,5"|}

let compiled_names modules =
  Dartea.Compiler.compile_modules ~entry:None (Project.Sources.of_list modules)
  |> Node_runner.output_of
  |> List.map (fun (module_ : Dartea.Compiler.artifact) -> module_.module_name)

let test_an_alias_inside_a_constructor _ =
  assert_runs
    ~modules:
      [
        source "Main.elm"
          {|
module Main exposing (used)

type T = T

type alias V = T

type Box a = Box (V -> a)

open : Box a -> V -> a
open box given =
    case box of
        Box inside -> inside given

used : String
used = open (Box (\_ -> "kept")) T
|};
      ]
    ~expr:"Main.used" ~expected:{|"kept"|}

let test_json_round_trip _ =
  assert_runs
    ~modules:
      [
        source "Main.elm"
          {|
module Main exposing (report)

import Json.Decode as D
import Json.Encode as E

pair : D.Decoder ( String, Int )
pair =
    D.map2 (\found age -> ( found, age ))
        (D.field "name" D.string)
        (D.field "age" D.int)

report : String
report =
    let
        text = E.encode (E.object [ ( "name", E.string "ann" ), ( "age", E.int 7 ) ])
    in
    case D.decodeString pair text of
        Ok found -> Tuple.first found ++ "/" ++ String.fromInt (Tuple.second found)
        Err problem -> "err " ++ D.errorToString problem
|};
      ]
    ~expr:"Main.report" ~expected:{|"ann/7"|}

let test_json_reports_the_path _ =
  assert_runs
    ~modules:
      [
        source "Main.elm"
          {|
module Main exposing (problem)

import Json.Decode as D

problem : String
problem =
    case D.decodeString (D.field "age" D.int) "{\"age\":\"seven\"}" of
        Ok _ -> "unexpected"
        Err found -> D.errorToString found
|};
      ]
    ~expr:"Main.problem"
    ~expected:
      {|"At field `age`:\n    Expecting an INT, but instead got: \"seven\""|}

let test_a_syntax_error_is_reported_not_raised _ =
  let outcome =
    Dartea.Compiler.compile_modules ~entry:None
      (Project.Sources.of_list
         [ source "Main.elm" "module Main exposing (..)\n\nmain = (\n" ])
  in
  assert_bool "a syntax error produced no report" (outcome.errors <> [])

let test_unimported_prelude_module_stays_out _ =
  let names =
    compiled_names
      [ source "Main.elm" "module Main exposing (main)\n\nmain : Int\nmain = 1\n" ]
  in
  assert_bool "Dict was emitted without an import" (not (List.mem "Dict" names))

let test_imported_prelude_module_comes_along _ =
  let names =
    compiled_names
      [
        source "Main.elm"
          "module Main exposing (main)\n\nimport Dict\n\nmain : Int\nmain = Dict.size Dict.empty\n";
      ]
  in
  assert_bool "Dict was left out despite the import" (List.mem "Dict" names)

let test_dict_needs_an_import _ =
  let outcome =
    Dartea.Compiler.compile_modules ~entry:None
      (Project.Sources.of_list
         [
           source "Main.elm"
             {|
module Main exposing (main)

main : Int
main = Dict.size Dict.empty
|};
         ])
  in
  assert_bool "Dict was reachable without an import" (outcome.errors <> [])

let test_html_builds_a_page _ =
  assert_runs
    ~modules:
      [
        source "Main.elm"
          {|
module Main exposing (page)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)

page : Html msg
page =
    div [ class "box" ] [ text "hi" ]
|};
      ]
    ~expr:"Main.page"
    ~expected:
      {|{"TAG":"node","tag":"div","attributes":[{"TAG":"property","key":"className","value":"box"}],"children":[{"TAG":"text","text":"hi"}]}|}

let test_html_needs_an_import _ =
  let outcome =
    Dartea.Compiler.compile_modules ~entry:None
      (Project.Sources.of_list
         [
           source "Main.elm"
             "module Main exposing (page)\n\npage = Html.text \"hi\"\n";
         ])
  in
  assert_bool "Html was reachable without an import" (outcome.errors <> [])

let test_virtual_dom_builds_a_tree _ =
  assert_runs
    ~modules:
      [
        source "Main.elm"
          {|
module Main exposing (page)

import VirtualDom

page : VirtualDom.Node msg
page =
    VirtualDom.node "div"
        [ VirtualDom.attribute "id" "main" ]
        [ VirtualDom.text "hi" ]
|};
      ]
    ~expr:"Main.page"
    ~expected:
      {|{"TAG":"node","tag":"div","attributes":[{"TAG":"attribute","key":"id","value":"main"}],"children":[{"TAG":"text","text":"hi"}]}|}

let test_a_partly_applied_platform_kernel _ =
  assert_runs
    ~modules:
      [
        source "Main.elm"
          {|
module Main exposing (page)

import VirtualDom

div : List (VirtualDom.Attribute msg) -> List (VirtualDom.Node msg) -> VirtualDom.Node msg
div =
    VirtualDom.node "div"

page : VirtualDom.Node msg
page =
    div [] []
|};
      ]
    ~expr:"Main.page"
    ~expected:{|{"TAG":"node","tag":"div","attributes":[],"children":[]}|}

let test_virtual_dom_text_builds_a_node _ =
  assert_runs
    ~modules:
      [
        source "Main.elm"
          {|
module Main exposing (hello)

import VirtualDom

hello : VirtualDom.Node msg
hello =
    VirtualDom.text "hi"
|};
      ]
    ~expr:"Main.hello" ~expected:{|{"TAG":"text","text":"hi"}|}

let test_dict_runs _ =
  assert_runs
    ~modules:
      [
        source "Main.elm"
          {|
module Main exposing (main)

import Dict

main : Int
main =
    Dict.size (Dict.insert 3 "c" (Dict.fromList [ ( 1, "a" ), ( 2, "b" ) ]))
|};
      ]
    ~expr:"Main.main" ~expected:"3"

let suite =
  [
    "dict_runs" >:: test_dict_runs;
    "virtual_dom_text_builds_a_node" >:: test_virtual_dom_text_builds_a_node;
    "virtual_dom_builds_a_tree" >:: test_virtual_dom_builds_a_tree;
    "html_builds_a_page" >:: test_html_builds_a_page;
    "html_needs_an_import" >:: test_html_needs_an_import;
    "a_partly_applied_platform_kernel" >:: test_a_partly_applied_platform_kernel;
    "an_alias_inside_a_constructor" >:: test_an_alias_inside_a_constructor;
    "json_round_trip" >:: test_json_round_trip;
    "json_reports_the_path" >:: test_json_reports_the_path;
    "a_syntax_error_is_reported_not_raised"
    >:: test_a_syntax_error_is_reported_not_raised;
    "unimported_prelude_module_stays_out"
    >:: test_unimported_prelude_module_stays_out;
    "imported_prelude_module_comes_along"
    >:: test_imported_prelude_module_comes_along;
    "dict_needs_an_import" >:: test_dict_needs_an_import;
    "qualified, exposed and aliased imports"
    >:: test_qualified_exposed_and_aliased_imports;
    "imported ctor survives the boundary"
    >:: test_imported_ctor_string_survives_the_boundary;
    "pattern match on imported ctor with payload"
    >:: test_pattern_match_on_imported_ctor_with_payload;
    "qualified constructor in a pattern"
    >:: test_qualified_constructor_in_a_pattern;
    "record alias constructor crosses the boundary"
    >:: test_record_alias_constructor_crosses_the_boundary;
    "runtime module is shared" >:: test_runtime_module_is_shared;
    "diamond imports" >:: test_diamond_imports;
    "imported arity crosses the boundary"
    >:: test_imported_arity_crosses_the_boundary;
  ]
