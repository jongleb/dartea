open OUnit2

let canonical input =
  match Parse.Main.parse input with
  | Error e -> raise e
  | Ok impl_list ->
      Canonical.Module.of_frontend ~fallback_name:"Main"
        (Ast.Kind.Frontend.Module.of_impl impl_list)

let resolve ~dependencies source =
  Canonicalization.Resolve_names.in_module
    ~dependencies:(List.map canonical dependencies)
    (canonical source)

let resolved ~dependencies source =
  match resolve ~dependencies source with
  | Ok module_ -> module_
  | Error errors ->
      assert_failure
        (String.concat "\n"
           (List.map Canonicalization.Resolve_names.show_error errors))

let failing ~dependencies source =
  match resolve ~dependencies source with
  | Ok _ -> assert_failure "expected resolution to fail"
  | Error errors -> errors

let expr_of module_ name =
  match
    Canonical.Module.String_map.find_opt name
      module_.Canonical.Module.top_declarations
  with
  | Some (d : Canonical.Declaration.t) -> Data.Located.unwrap d.body_part.expr
  | None -> assert_failure (Printf.sprintf "declaration %s not found" name)

let signature_of module_ name =
  match
    Canonical.Module.String_map.find_opt name
      module_.Canonical.Module.top_declarations
  with
  | Some (d : Canonical.Declaration.t) ->
      Option.map
        (fun (tp : Canonical.Declaration.type_part) -> tp.type_alias)
        d.type_part_data
  | None -> assert_failure (Printf.sprintf "declaration %s not found" name)

let errors_printer errors =
  String.concat "\n" (List.map Canonicalization.Resolve_names.show_error errors)

let list_module =
  {|
module Data.List exposing (map)

map f = f
|}

let color_module =
  {|
module Paint exposing (Color(..), Thing)

type Color = Red | Blue

type Thing = Thing
|}

let test_dependency_order _ =
  let modules =
    List.map canonical
      [
        {|
module Main exposing (..)

import Middle

x = 1
|};
        {|
module Middle exposing (..)

import Leaf

y = 2
|};
        {|
module Leaf exposing (..)

z = 3
|};
      ]
  in
  match Canonicalization.Module_graph.in_dependency_order modules with
  | Error e ->
      assert_failure (Canonicalization.Module_graph.show_error e)
  | Ok ordered ->
      assert_equal
        ~printer:(String.concat ", ")
        [ "Leaf"; "Middle"; "Main" ]
        (List.map (fun (m : Canonical.Module.t) -> m.name) ordered)

let test_import_cycle _ =
  let modules =
    List.map canonical
      [
        {|
module Main exposing (..)

import Other

x = 1
|};
        {|
module Other exposing (..)

import Main

y = 2
|};
      ]
  in
  match Canonicalization.Module_graph.in_dependency_order modules with
  | Ok _ -> assert_failure "expected an import cycle"
  | Error e ->
      assert_equal
        ~printer:Canonicalization.Module_graph.show_error
        (Canonicalization.Module_graph.Import_cycle [ "Main"; "Other" ])
        e

let test_alias_is_the_real_module _ =
  let module_ =
    resolved ~dependencies:[ list_module ]
      {|
module Main exposing (..)

import Data.List as L

x = L.map
|}
  in
  assert_equal ~printer:Canonical.Expr.show
    (Canonical.Expr.Expr_ident
       (Data.Name.global ~module_name:"Data.List" ~exported_name:"map"))
    (expr_of module_ "x")

let test_exposed_name_becomes_global _ =
  let module_ =
    resolved ~dependencies:[ list_module ]
      {|
module Main exposing (..)

import Data.List exposing (map)

x = map
|}
  in
  assert_equal ~printer:Canonical.Expr.show
    (Canonical.Expr.Expr_ident
       (Data.Name.global ~module_name:"Data.List" ~exported_name:"map"))
    (expr_of module_ "x")

let test_exposing_all_uses_dependency_exports _ =
  let module_ =
    resolved ~dependencies:[ list_module ]
      {|
module Main exposing (..)

import Data.List exposing (..)

x = map
|}
  in
  assert_equal ~printer:Canonical.Expr.show
    (Canonical.Expr.Expr_ident
       (Data.Name.global ~module_name:"Data.List" ~exported_name:"map"))
    (expr_of module_ "x")

let test_local_declaration_shadows_import _ =
  let module_ =
    resolved ~dependencies:[ list_module ]
      {|
module Main exposing (..)

import Data.List exposing (map)

map a = a

x = map
|}
  in
  assert_equal ~printer:Canonical.Expr.show
    (Canonical.Expr.Expr_ident (Data.Name.local "map"))
    (expr_of module_ "x")

let test_parameter_shadows_import _ =
  let module_ =
    resolved ~dependencies:[ list_module ]
      {|
module Main exposing (..)

import Data.List exposing (map)

x map = map
|}
  in
  assert_equal ~printer:Canonical.Expr.show
    (Canonical.Expr.Expr_ident (Data.Name.local "map"))
    (expr_of module_ "x")

let test_builtins_stay_local _ =
  let module_ =
    resolved ~dependencies:[ list_module ]
      {|
module Main exposing (..)

import Data.List exposing (map)

x = length "hello"
|}
  in
  assert_equal ~printer:Canonical.Expr.show
    (Canonical.Expr.Expr_apply
       {
         fn = Canonical.Expr.Expr_ident (Data.Name.local "length");
         arg = Canonical.Expr.Expr_string "hello";
       })
    (expr_of module_ "x")

let test_exposed_ctor_in_pattern _ =
  let module_ =
    resolved ~dependencies:[ color_module ]
      {|
module Main exposing (..)

import Paint exposing (Color(..))

x =
    case Red of
        Red -> 1
        Blue -> 2
|}
  in
  let global name =
    Data.Name.global ~module_name:"Paint" ~exported_name:name
  in
  assert_equal ~printer:Canonical.Expr.show
    (Canonical.Expr.Expr_pattern
       {
         expr = Canonical.Expr.Expr_ident (global "Red");
         pattern_data_items =
           [
             {
               pattern = Canonical.Pattern.P_ctor (global "Red", []);
               expr = Canonical.Expr.Expr_int 1;
             };
             {
               pattern = Canonical.Pattern.P_ctor (global "Blue", []);
               expr = Canonical.Expr.Expr_int 2;
             };
           ];
       })
    (expr_of module_ "x")

let test_qualified_type_in_signature _ =
  let module_ =
    resolved ~dependencies:[ color_module ]
      {|
module Main exposing (..)

import Paint

x : Paint.Thing -> Int
x thing = 1
|}
  in
  let expected =
    Canonical.Typedef.Impl.
      {
        parameters = [];
        body =
          Canonical.Typedef.Kind.Tkind_function
            {
              arguments =
                [
                  {
                    parameters = [];
                    body =
                      Canonical.Typedef.Kind.Tkind_concrete
                        (Data.Located.dummy
                           (Data.Name.global ~module_name:"Paint"
                              ~exported_name:"Thing"));
                  };
                  {
                    parameters = [];
                    body =
                      Canonical.Typedef.Kind.Tkind_concrete
                        (Data.Located.dummy (Data.Name.local "Int"));
                  };
                ];
            };
      }
  in
  assert_equal
    ~printer:(function
      | None -> "<no signature>"
      | Some typedef -> Canonical.Typedef.Impl.show typedef)
    (Some expected)
    (signature_of module_ "x")

let test_unknown_module _ =
  let errors =
    failing ~dependencies:[]
      {|
module Main exposing (..)

x = Missing.foo
|}
  in
  assert_equal ~printer:errors_printer
    [
      {
        Canonicalization.Resolve_names.origin =
          Value_declaration (Data.Located.dummy "x");
        problem = Unknown_module { qualifier = "Missing" };
      };
    ]
    errors

let test_unknown_imported_module _ =
  let errors =
    failing ~dependencies:[]
      {|
module Main exposing (..)

import Missing

x = 1
|}
  in
  assert_equal ~printer:errors_printer
    [
      {
        Canonicalization.Resolve_names.origin = Import "Missing";
        problem = Unknown_module { qualifier = "Missing" };
      };
    ]
    errors

let test_not_exposed _ =
  let errors =
    failing ~dependencies:[ list_module ]
      {|
module Main exposing (..)

import Data.List

x = Data.List.secret
|}
  in
  assert_equal ~printer:errors_printer
    [
      {
        Canonicalization.Resolve_names.origin =
          Value_declaration (Data.Located.dummy "x");
        problem = Not_exposed { module_name = "Data.List"; name = "secret" };
      };
    ]
    errors

let test_import_of_hidden_name _ =
  let errors =
    failing ~dependencies:[ list_module ]
      {|
module Main exposing (..)

import Data.List exposing (filter)

x = 1
|}
  in
  assert_equal ~printer:errors_printer
    [
      {
        Canonicalization.Resolve_names.origin = Import "Data.List";
        problem = Not_exposed { module_name = "Data.List"; name = "filter" };
      };
    ]
    errors

let test_ambiguous_unqualified _ =
  let errors =
    failing
      ~dependencies:
        [
          {|
module First exposing (thing)

thing = 1
|};
          {|
module Second exposing (thing)

thing = 2
|};
        ]
      {|
module Main exposing (..)

import First exposing (thing)
import Second exposing (thing)

x = thing
|}
  in
  assert_equal ~printer:errors_printer
    [
      {
        Canonicalization.Resolve_names.origin =
          Value_declaration (Data.Located.dummy "x");
        problem =
          Ambiguous { name = "thing"; modules = [ "First"; "Second" ] };
      };
    ]
    errors

let test_duplicate_constructor _ =
  let errors =
    failing ~dependencies:[]
      {|
module Main exposing (..)

type Alpha = Shared

type Beta = Shared
|}
  in
  assert_equal ~printer:errors_printer
    [
      {
        Canonicalization.Resolve_names.origin = Type_declaration "Beta";
        problem = Duplicate_declaration { name = "Shared" };
      };
    ]
    errors

let test_ctors_not_exposed _ =
  let errors =
    failing
      ~dependencies:
        [
          {|
module Paint exposing (Color)

type Color = Red | Blue
|};
        ]
      {|
module Main exposing (..)

import Paint exposing (Color(..))

x = 1
|}
  in
  assert_equal ~printer:errors_printer
    [
      {
        Canonicalization.Resolve_names.origin = Import "Paint";
        problem =
          Ctors_not_exposed { module_name = "Paint"; type_name = "Color" };
      };
    ]
    errors

let test_let_binding_shadows_import _ =
  let module_ =
    resolved ~dependencies:[ list_module ]
      {|
module Main exposing (..)

import Data.List exposing (map)

x =
    let
        map = 1
    in
    map
|}
  in
  assert_equal ~printer:Canonical.Expr.show
    (Canonical.Expr.Expr_let
       {
         binding =
           {
             bind_body =
               {
                 name = Data.Located.dummy "map";
                 body = Canonical.Expr.Expr_int 1;
               };
           };
         body = Canonical.Expr.Expr_ident (Data.Name.local "map");
       })
    (expr_of module_ "x")

let test_pattern_binding_shadows_import _ =
  let module_ =
    resolved ~dependencies:[ list_module ]
      {|
module Main exposing (..)

import Data.List exposing (map)

x =
    case 1 of
        map -> map
|}
  in
  assert_equal ~printer:Canonical.Expr.show
    (Canonical.Expr.Expr_pattern
       {
         expr = Canonical.Expr.Expr_int 1;
         pattern_data_items =
           [
             {
               pattern = Canonical.Pattern.P_var "map";
               expr = Canonical.Expr.Expr_ident (Data.Name.local "map");
             };
           ];
       })
    (expr_of module_ "x")

let test_alias_hides_the_full_module_name _ =
  let errors =
    failing ~dependencies:[ list_module ]
      {|
module Main exposing (..)

import Data.List as L

x = Data.List.map
|}
  in
  assert_equal ~printer:errors_printer
    [
      {
        Canonicalization.Resolve_names.origin =
          Value_declaration (Data.Located.dummy "x");
        problem = Unknown_module { qualifier = "Data.List" };
      };
    ]
    errors

let test_type_and_alias_share_a_name _ =
  let errors =
    failing ~dependencies:[]
      {|
module Main exposing (..)

type Shape = Circle

type alias Shape = Int
|}
  in
  assert_equal ~printer:errors_printer
    [
      {
        Canonicalization.Resolve_names.origin = Type_alias "Shape";
        problem = Duplicate_declaration { name = "Shape" };
      };
    ]
    errors

let test_unqualified_use_without_exposing_stays_local _ =
  let module_ =
    resolved ~dependencies:[ list_module ]
      {|
module Main exposing (..)

import Data.List

x = map
|}
  in
  assert_equal ~printer:Canonical.Expr.show
    (Canonical.Expr.Expr_ident (Data.Name.local "map"))
    (expr_of module_ "x")

let test_diamond_dependency_order _ =
  let modules =
    List.map canonical
      [
        {|
module Main exposing (..)

import Left
import Right

x = 1
|};
        {|
module Left exposing (..)

import Shared

y = 2
|};
        {|
module Right exposing (..)

import Shared

z = 3
|};
        {|
module Shared exposing (..)

w = 4
|};
      ]
  in
  match Canonicalization.Module_graph.in_dependency_order modules with
  | Error e -> assert_failure (Canonicalization.Module_graph.show_error e)
  | Ok ordered ->
      assert_equal
        ~printer:(String.concat ", ")
        [ "Shared"; "Left"; "Right"; "Main" ]
        (List.map (fun (m : Canonical.Module.t) -> m.name) ordered)

let test_unknown_import_is_not_an_edge _ =
  let modules =
    List.map canonical
      [
        {|
module Main exposing (..)

import Missing

x = 1
|};
      ]
  in
  match Canonicalization.Module_graph.in_dependency_order modules with
  | Error e -> assert_failure (Canonicalization.Module_graph.show_error e)
  | Ok ordered ->
      assert_equal
        ~printer:(String.concat ", ")
        [ "Main" ]
        (List.map (fun (m : Canonical.Module.t) -> m.name) ordered)

let suite =
  [
    "dependency order" >:: test_dependency_order;
    "import cycle" >:: test_import_cycle;
    "alias is the real module" >:: test_alias_is_the_real_module;
    "exposed name becomes global" >:: test_exposed_name_becomes_global;
    "exposing (..) uses dependency exports"
    >:: test_exposing_all_uses_dependency_exports;
    "local declaration shadows import"
    >:: test_local_declaration_shadows_import;
    "parameter shadows import" >:: test_parameter_shadows_import;
    "builtins stay local" >:: test_builtins_stay_local;
    "exposed ctor in pattern" >:: test_exposed_ctor_in_pattern;
    "qualified type in signature" >:: test_qualified_type_in_signature;
    "unknown module" >:: test_unknown_module;
    "unknown imported module" >:: test_unknown_imported_module;
    "not exposed" >:: test_not_exposed;
    "import of hidden name" >:: test_import_of_hidden_name;
    "ambiguous unqualified" >:: test_ambiguous_unqualified;
    "duplicate constructor" >:: test_duplicate_constructor;
    "constructors not exposed" >:: test_ctors_not_exposed;
    "let binding shadows import" >:: test_let_binding_shadows_import;
    "pattern binding shadows import" >:: test_pattern_binding_shadows_import;
    "alias hides the full module name"
    >:: test_alias_hides_the_full_module_name;
    "type and alias share a name" >:: test_type_and_alias_share_a_name;
    "unqualified use without exposing stays local"
    >:: test_unqualified_use_without_exposing_stays_local;
    "diamond dependency order" >:: test_diamond_dependency_order;
    "unknown import is not an edge" >:: test_unknown_import_is_not_an_edge;
  ]
