open OUnit2

let canonical input =
  match Parse.Main.parse input with
  | Error e -> raise e
  | Ok impl_list ->
      Canonical.Module.of_frontend ~fallback_name:"Main"
        (Ast.Kind.Frontend.Module.of_impl impl_list)

let resolved ~dependencies module_ =
  match Canonicalization.Resolve_names.in_module ~dependencies module_ with
  | Ok resolved -> resolved
  | Error errors ->
      assert_failure
        (String.concat "\n"
           (List.map Canonicalization.Resolve_names.show_error errors))

let inferred ~imports module_ =
  Infer.Infer_proc.Fresh.reset ();
  Infer.Infer_proc.infer_toplevel ~imports module_ Dartea.Compiler.initial_ctx

let published source =
  let module_ = resolved ~dependencies:[] (canonical source) in
  ( module_,
    Infer.Infer_proc.interface_of module_ (inferred ~imports:[] module_) )

let importing_all ~dependencies source =
  let published = List.map published dependencies in
  let module_ =
    resolved ~dependencies:(List.map fst published) (canonical source)
  in
  inferred ~imports:(List.map snd published) module_

let importing ~dependency source =
  importing_all ~dependencies:[ dependency ] source

let type_of result name =
  match
    Infer.Infer_proc.Name_map.find_opt (Data.Name.local name)
      result.Infer.Infer_proc.ctx
  with
  | Some (Typed.Type.Scheme (_, ty)) -> ty
  | None -> assert_failure (Printf.sprintf "no type inferred for %s" name)

let global module_name exported_name =
  Data.Name.global ~module_name ~exported_name

let value_named interface name =
  List.find_opt
    (fun (value : Interface.value) -> Data.Name.equal value.name name)
    interface.Interface.values

let type_named interface name =
  List.find_opt
    (fun (td : Canonical.Typedecl.t) -> Data.Name.equal td.name name)
    interface.Interface.types

let ctor_count interface name =
  type_named interface name
  |> Option.map (fun (td : Canonical.Typedecl.t) -> List.length td.ctors)

let counted = function None -> "<absent>" | Some n -> string_of_int n

let paint =
  {|
module Paint exposing (Color(..), red)

type Color = Red | Blue

red : Color
red = Red
|}

let arithmetic =
  {|
module Arithmetic exposing (double)

double : Int -> Int
double n = n + n

secret n = n
|}

let exported_value_names interface =
  List.map
    (fun (value : Interface.value) -> Data.Name.to_string value.name)
    interface.Interface.values

let test_exported_values_only _ =
  let _, interface = published arithmetic in
  assert_equal ~printer:(String.concat ", ") [ "Arithmetic.double" ]
    (exported_value_names interface)

let test_value_scheme_is_globalized _ =
  let _, interface = published paint in
  let scheme =
    value_named interface (global "Paint" "red")
    |> Option.map (fun (value : Interface.value) ->
           match value.scheme with Typed.Type.Scheme (_, ty) -> ty)
  in
  assert_equal
    ~printer:(function
      | None -> "<absent>" | Some ty -> Typed.Type.show ty)
    (Some (Typed.Type.TCustom (global "Paint" "Color", [])))
    scheme

let test_type_and_ctors_are_globalized _ =
  let _, interface = published paint in
  let color = type_named interface (global "Paint" "Color") in
  assert_equal
    ~printer:(function
      | None -> "<absent>"
      | Some (td : Canonical.Typedecl.t) ->
          Data.Name.show td.name ^ " = "
          ^ String.concat " | "
              (List.map
                 (fun (c : Canonical.Typedecl.type_ctor) ->
                   Data.Name.show c.id)
                 td.ctors))
    (Some
       {
         Canonical.Typedecl.name = global "Paint" "Color";
         params = [];
         ctors =
           [
             { id = global "Paint" "Red"; data = [] };
             { id = global "Paint" "Blue"; data = [] };
           ];
       })
    color

let test_type_without_ctors_keeps_them_private _ =
  let _, interface =
    published {|
module Paint exposing (Color)

type Color = Red | Blue
|}
  in
  assert_equal ~printer:counted (Some 0)
    (ctor_count interface (global "Paint" "Color"))

let test_unexposed_type_is_absent _ =
  let _, interface =
    published
      {|
module Paint exposing (red)

type Color = Red | Blue

red = Red
|}
  in
  assert_equal ~printer:string_of_int 0
    (List.length interface.Interface.types)

let test_exposed_value_is_typed_through_the_import _ =
  let result =
    importing ~dependency:arithmetic
      {|
module Main exposing (..)

import Arithmetic exposing (double)

x = double 21
|}
  in
  assert_equal ~printer:Typed.Type.show Typed.Type.TInt (type_of result "x")

let test_qualified_value_is_typed_through_the_import _ =
  let result =
    importing ~dependency:arithmetic
      {|
module Main exposing (..)

import Arithmetic

x = Arithmetic.double 21
|}
  in
  assert_equal ~printer:Typed.Type.show Typed.Type.TInt (type_of result "x")

let test_imported_ctor_has_the_owners_type _ =
  let result =
    importing ~dependency:paint
      {|
module Main exposing (..)

import Paint exposing (Color(..))

x = Red
|}
  in
  assert_equal ~printer:Typed.Type.show
    (Typed.Type.TCustom (global "Paint" "Color", []))
    (type_of result "x")

let test_imported_value_keeps_its_type _ =
  let result =
    importing ~dependency:paint
      {|
module Main exposing (..)

import Paint exposing (red)

x = red
|}
  in
  assert_equal ~printer:Typed.Type.show
    (Typed.Type.TCustom (global "Paint" "Color", []))
    (type_of result "x")

let test_case_over_imported_ctors _ =
  let result =
    importing ~dependency:paint
      {|
module Main exposing (..)

import Paint exposing (Color(..))

x : Int
x =
    case Red of
        Red -> 1
        Blue -> 2
|}
  in
  assert_equal ~printer:Typed.Type.show Typed.Type.TInt (type_of result "x")

let test_imported_type_in_a_signature _ =
  let result =
    importing ~dependency:paint
      {|
module Main exposing (..)

import Paint exposing (Color(..))

x : Paint.Color
x = Blue
|}
  in
  assert_equal ~printer:Typed.Type.show
    (Typed.Type.TCustom (global "Paint" "Color", []))
    (type_of result "x")

let test_imported_siblings_reach_the_consumer _ =
  let result =
    importing ~dependency:paint
      {|
module Main exposing (..)

import Paint exposing (Color(..))

x = Red
|}
  in
  let siblings =
    Infer.Infer_proc.Name_map.find_opt (global "Paint" "Red")
      result.Infer.Infer_proc.siblings_env
  in
  assert_equal
    ~printer:(function
      | None -> "<absent>"
      | Some entries ->
          String.concat ", "
            (List.map
               (fun (name, arity) ->
                 Printf.sprintf "%s/%d" (Data.Name.to_string name) arity)
               entries))
    (Some [ (global "Paint" "Red", 0); (global "Paint" "Blue", 0) ])
    siblings

let test_imported_type_alias_expands _ =
  let result =
    importing
      ~dependency:
        {|
module Units exposing (Meters, zero)

type alias Meters = Int

zero = 0
|}
      {|
module Main exposing (..)

import Units exposing (Meters)

x : Meters
x = 3
|}
  in
  assert_equal ~printer:Typed.Type.show Typed.Type.TInt (type_of result "x")

let boxes =
  {|
module Boxes exposing (Box(..), Color(..))

type Color = Red | Blue

type Box = Box Color
|}

let test_ctor_payload_type_is_globalized _ =
  let result =
    importing ~dependency:boxes
      {|
module Main exposing (..)

import Boxes exposing (Box(..), Color(..))

x = Box Red
|}
  in
  assert_equal ~printer:Typed.Type.show
    (Typed.Type.TCustom (global "Boxes" "Box", []))
    (type_of result "x")

let test_imported_ctor_arity_reaches_the_consumer _ =
  let result =
    importing ~dependency:boxes
      {|
module Main exposing (..)

import Boxes exposing (Box(..), Color(..))

x = Box Blue
|}
  in
  let arity_of name =
    List.find_opt
      (fun (c : Infer.Infer_proc.ctor_info) -> Data.Name.equal c.name name)
      result.Infer.Infer_proc.constructors
    |> Option.map (fun (c : Infer.Infer_proc.ctor_info) -> c.arity)
  in
  assert_equal
    ~printer:(function None -> "<absent>" | Some n -> string_of_int n)
    (Some 1)
    (arity_of (global "Boxes" "Box"))

let test_alias_body_is_globalized _ =
  let result =
    importing
      ~dependency:
        {|
module Paint exposing (Color(..), Shade)

type Color = Red | Blue

type alias Shade = Color
|}
      {|
module Main exposing (..)

import Paint exposing (Color(..), Shade)

x : Shade
x = Red
|}
  in
  assert_equal ~printer:Typed.Type.show
    (Typed.Type.TCustom (global "Paint" "Color", []))
    (type_of result "x")

let test_open_module_exports_everything _ =
  let _, interface =
    published
      {|
module Open exposing (..)

type Shade = Light | Dark

pick = Light
|}
  in
  assert_equal ~printer:(String.concat ", ") [ "Open.pick" ]
    (exported_value_names interface);
  assert_equal ~printer:counted (Some 2)
    (ctor_count interface (global "Open" "Shade"))

let test_two_imports_at_once _ =
  let result =
    importing_all
      ~dependencies:[ arithmetic; paint ]
      {|
module Main exposing (..)

import Arithmetic exposing (double)
import Paint exposing (red)

x = double 1

y = red
|}
  in
  assert_equal ~printer:Typed.Type.show Typed.Type.TInt (type_of result "x");
  assert_equal ~printer:Typed.Type.show
    (Typed.Type.TCustom (global "Paint" "Color", []))
    (type_of result "y")

let suite =
  [
    "exported values only" >:: test_exported_values_only;
    "value scheme is globalized" >:: test_value_scheme_is_globalized;
    "type and ctors are globalized" >:: test_type_and_ctors_are_globalized;
    "type without ctors keeps them private"
    >:: test_type_without_ctors_keeps_them_private;
    "unexposed type is absent" >:: test_unexposed_type_is_absent;
    "exposed value is typed through the import"
    >:: test_exposed_value_is_typed_through_the_import;
    "qualified value is typed through the import"
    >:: test_qualified_value_is_typed_through_the_import;
    "imported ctor has the owner's type"
    >:: test_imported_ctor_has_the_owners_type;
    "imported value keeps its type" >:: test_imported_value_keeps_its_type;
    "case over imported ctors" >:: test_case_over_imported_ctors;
    "imported type in a signature" >:: test_imported_type_in_a_signature;
    "imported siblings reach the consumer"
    >:: test_imported_siblings_reach_the_consumer;
    "imported type alias expands" >:: test_imported_type_alias_expands;
    "ctor payload type is globalized" >:: test_ctor_payload_type_is_globalized;
    "imported ctor arity reaches the consumer"
    >:: test_imported_ctor_arity_reaches_the_consumer;
    "alias body is globalized" >:: test_alias_body_is_globalized;
    "open module exports everything" >:: test_open_module_exports_everything;
    "two imports at once" >:: test_two_imports_at_once;
  ]
