open OUnit2

let source = Node_runner.source

let node_eval ~modules ~expr =
  Node_runner.evaluate ~compiled:(Dartea.Compiler.compile_modules modules) ~expr

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

let suite =
  [
    "qualified, exposed and aliased imports"
    >:: test_qualified_exposed_and_aliased_imports;
    "imported ctor survives the boundary"
    >:: test_imported_ctor_string_survives_the_boundary;
    "pattern match on imported ctor with payload"
    >:: test_pattern_match_on_imported_ctor_with_payload;
    "runtime module is shared" >:: test_runtime_module_is_shared;
    "diamond imports" >:: test_diamond_imports;
  ]
