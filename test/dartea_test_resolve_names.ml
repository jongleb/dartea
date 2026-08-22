open OUnit2

let resolve ~dependencies source =
  Canonicalization.Resolve_names.in_module
    ~dependencies:(List.map Utils.canonical dependencies)
    (Utils.canonical source)

let resolved ~dependencies source =
  match resolve ~dependencies source with
  | Ok module_ -> module_
  | Error errors ->
      assert_failure
        (String.concat "\n"
           (List.map Reporting.Error.show errors))

let without_suggestions (problem : Reporting.Error.problem) =
  match problem with
  | Name (Unbound_value found) ->
      Reporting.Error.Name (Unbound_value { found with near = [] })
  | Name (Unknown_constructor found) ->
      Name (Unknown_constructor { found with near = [] })
  | Name (Unknown_type found) -> Name (Unknown_type { found with near = [] })
  | Name (Unknown_module found) -> Name (Unknown_module { found with near = [] })
  | Name (Not_exposed found) -> Name (Not_exposed { found with near = [] })
  | Name
      ( Ctors_not_exposed _ | Ambiguous _ | Unknown_kernel _
      | Kernel_needs_annotation _ | Kernel_arity_mismatch _
      | Duplicate_declaration _ | Duplicate_binder _ | Import_cycle _
      | Recursive_value _ )
  | Type _ | Syntax _ | Project _ ->
      problem

let failing ~dependencies source =
  match resolve ~dependencies source with
  | Ok _ -> assert_failure "expected resolution to fail"
  | Error errors ->
      List.map
        (fun (error : Reporting.Error.t) -> without_suggestions error.problem)
        errors

let declaration_named module_ name =
  List.find_opt
    (fun (d : Canonical.Declaration.t) ->
      String.equal (Data.Located.unwrap d.body_part.name) name)
    module_.Canonical.Module.top_declarations

let expr_of module_ name =
  match declaration_named module_ name with
  | Some (d : Canonical.Declaration.t) ->
      Utils.Canonical_expr_util.dummify d.body_part.expr
  | None -> assert_failure (Printf.sprintf "declaration %s not found" name)

let signature_of module_ name =
  match declaration_named module_ name with
  | Some (d : Canonical.Declaration.t) ->
      Option.map
        (fun (tp : Canonical.Declaration.type_part) ->
          Utils.Canonical_typedef_util.dummify tp.type_alias)
        d.type_part_data
  | None -> assert_failure (Printf.sprintf "declaration %s not found" name)

let errors_printer problems =
  String.concat "\n" (List.map Reporting.Error.show_problem problems)

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
    List.map Utils.canonical
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
    List.map Utils.canonical
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
    (Data.Located.dummy(Canonical.Expr.Expr_ident
       (Data.Name.global ~module_name:"Data.List" ~exported_name:"map")))
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
    (Data.Located.dummy(Canonical.Expr.Expr_ident
       (Data.Name.global ~module_name:"Data.List" ~exported_name:"map")))
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
    (Data.Located.dummy(Canonical.Expr.Expr_ident
       (Data.Name.global ~module_name:"Data.List" ~exported_name:"map")))
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
    (Data.Located.dummy(Canonical.Expr.Expr_ident (Data.Name.local "map")))
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
    (Data.Located.dummy(Canonical.Expr.Expr_ident (Data.Name.local "map")))
    (expr_of module_ "x")

let test_operators_stay_local _ =
  let module_ =
    resolved ~dependencies:[ list_module ]
      {|
module Main exposing (..)

import Data.List exposing (map)

x = 1 + 2
|}
  in
  assert_equal ~printer:Canonical.Expr.show
    (Data.Located.dummy
       (Canonical.Expr.Expr_apply
          {
            fn =
              Data.Located.dummy
                (Canonical.Expr.Expr_apply
                   {
                     fn =
                       Data.Located.dummy
                         (Canonical.Expr.Expr_ident (Data.Name.local "+"));
                     arg = Data.Located.dummy (Canonical.Expr.Expr_int 1);
                   });
            arg = Data.Located.dummy (Canonical.Expr.Expr_int 2);
          }))
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
    (Data.Located.dummy(Canonical.Expr.Expr_pattern
       {
         expr = Data.Located.dummy(Canonical.Expr.Expr_ident (global "Red"));
         pattern_data_items =
           [
             {
               pattern = Data.Located.dummy(Canonical.Pattern.P_ctor (global "Red", []));
               expr = Data.Located.dummy(Canonical.Expr.Expr_int 1);
             };
             {
               pattern = Data.Located.dummy(Canonical.Pattern.P_ctor (global "Blue", []));
               expr = Data.Located.dummy(Canonical.Expr.Expr_int 2);
             };
           ];
       }))
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
              arguments = [ {
                    parameters = [];
                    body =
                      Canonical.Typedef.Kind.Tkind_concrete
                        (Data.Located.dummy
                           (Data.Name.global ~module_name:"Paint"
                              ~exported_name:"Thing"));
                  } ]; result = ({
                    parameters = [];
                    body =
                      Canonical.Typedef.Kind.Tkind_concrete
                        (Data.Located.dummy (Data.Name.local "Int"));
                  });
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
      Reporting.Error.Name
        (Unbound_value
           {
             name =
               Data.Name.global ~module_name:"Missing" ~exported_name:"foo";
             prefix = Unknown_prefix "Missing";
             near = [];
           });
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
      Reporting.Error.Name (Unknown_module { qualifier = "Missing"; near = [] });
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
      Reporting.Error.Name
        (Unbound_value
           {
             name =
               Data.Name.global ~module_name:"Data.List" ~exported_name:"secret";
             prefix = Known_prefix "Data.List";
             near = [];
           });
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
      Reporting.Error.Name (Not_exposed { module_name = "Data.List"; name = "filter"; near = [] });
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
      Reporting.Error.Name (Ambiguous { name = "thing"; modules = [ "First"; "Second" ] });
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
      Reporting.Error.Name (Duplicate_declaration { name = "Shared" });
    ]
    errors

let test_duplicate_top_level_declaration _ =
  let errors =
    failing ~dependencies:[]
      {|
module Main exposing (..)

foo = 1

bar = 2

foo = 3
|}
  in
  assert_equal ~printer:errors_printer
    [
      Reporting.Error.Name (Duplicate_declaration { name = "foo" });
    ]
    errors

let test_an_annotated_duplicate_is_still_a_duplicate _ =
  let errors =
    failing ~dependencies:[]
      {|
module Main exposing (..)

foo : Int
foo = 1

foo : Int
foo = 2
|}
  in
  assert_equal ~printer:errors_printer
    [
      Reporting.Error.Name (Duplicate_declaration { name = "foo" });
    ]
    errors

let test_declarations_keep_their_source_order _ =
  let module_ =
    resolved ~dependencies:[]
      {|
module Main exposing (..)

zebra = 1

alpha = 2

middle = 3
|}
  in
  assert_equal
    ~printer:(String.concat ", ")
    [ "zebra"; "alpha"; "middle" ]
    (List.map
       (fun (d : Canonical.Declaration.t) ->
         Data.Located.unwrap d.body_part.name)
       module_.Canonical.Module.top_declarations)

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
      Reporting.Error.Name (Ctors_not_exposed { module_name = "Paint"; type_name = "Color" });
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
    (Data.Located.dummy(Canonical.Expr.Expr_let
       {
         binding =
           {
             bind_type = None;
             bind_body =
               {
                 name = Data.Located.dummy "map";
                 body = Data.Located.dummy(Canonical.Expr.Expr_int 1);
               };
           };
         body = Data.Located.dummy(Canonical.Expr.Expr_ident (Data.Name.local "map"));
       }))
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
    (Data.Located.dummy(Canonical.Expr.Expr_pattern
       {
         expr = Data.Located.dummy(Canonical.Expr.Expr_int 1);
         pattern_data_items =
           [
             {
               pattern = Data.Located.dummy(Canonical.Pattern.P_var "map");
               expr = Data.Located.dummy(Canonical.Expr.Expr_ident (Data.Name.local "map"));
             };
           ];
       }))
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
      Reporting.Error.Name
        (Unbound_value
           {
             name =
               Data.Name.global ~module_name:"Data.List" ~exported_name:"map";
             prefix = Unknown_prefix "Data.List";
             near = [];
           });
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
      Reporting.Error.Name (Duplicate_declaration { name = "Shape" });
    ]
    errors

let test_unqualified_use_without_exposing_is_unbound _ =
  assert_equal ~printer:errors_printer
    [ Reporting.Error.Name (Unbound_value
           { name = Data.Name.local "map"; prefix = No_prefix; near = [] }) ]
    (failing ~dependencies:[ list_module ]
       {|
module Main exposing (..)

import Data.List

x = map
|})

let test_diamond_dependency_order _ =
  let modules =
    List.map Utils.canonical
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
    List.map Utils.canonical
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


let test_kernel_reference_becomes_a_kernel_node _ =
  let module_ = resolved ~dependencies:[] {|
module M exposing (length)

length : String -> Int
length = Elm.Kernel.String.length
|} in
  assert_equal ~printer:Canonical.Expr.show
    (Data.Located.dummy(Canonical.Expr.Expr_kernel (Data.Kernel.Unary Data.Kernel.String_length)))
    (expr_of module_ "length")

let problems_of problems = problems
let problems_printer = errors_printer

let test_unknown_kernel_reference_is_reported _ =
  assert_equal ~printer:problems_printer
    [ Reporting.Error.Name (Unknown_kernel { module_name = "Elm.Kernel.String"; exported_name = "reverse" }) ]
    (problems_of
       (failing ~dependencies:[] {|
module M exposing (bogus)

bogus : String -> String
bogus = Elm.Kernel.String.reverse
|}))

let test_kernel_without_a_signature_is_reported _ =
  assert_equal ~printer:problems_printer
    [ Reporting.Error.Name (Kernel_needs_annotation { name = "length" }) ]
    (problems_of
       (failing ~dependencies:[] {|
module M exposing (length)

length = Elm.Kernel.String.length
|}))

let test_kernel_arity_must_match_the_signature _ =
  assert_equal ~printer:problems_printer
    [ Reporting.Error.Name (Kernel_arity_mismatch { declared = 2; kernel = 1 }) ]
    (problems_of
       (failing ~dependencies:[] {|
module M exposing (length)

length : String -> String -> Int
length = Elm.Kernel.String.length
|}))

let test_a_name_bound_twice_in_one_pattern_is_rejected _ =
  let errors =
    failing ~dependencies:[]
      {|
module Main exposing (..)

good =
    case ( 1, 2 ) of
        ( a, a ) ->
            a
|}
  in
  assert_equal ~printer:errors_printer
    [
      Reporting.Error.Name (Duplicate_binder { name = "a" });
    ]
    errors

let test_a_parameter_bound_twice_is_rejected _ =
  let errors =
    failing ~dependencies:[]
      {|
module Main exposing (..)

good a a = a
|}
  in
  assert_equal ~printer:errors_printer
    [
      Reporting.Error.Name (Duplicate_binder { name = "a" });
    ]
    errors

let test_a_lambda_parameter_bound_twice_is_rejected _ =
  let errors =
    failing ~dependencies:[]
      {|
module Main exposing (..)

good = \a a -> a
|}
  in
  assert_equal ~printer:errors_printer
    [
      Reporting.Error.Name (Duplicate_binder { name = "a" });
    ]
    errors

let test_a_binder_may_shadow_an_outer_name _ =
  let module_ =
    resolved ~dependencies:[]
      {|
module Main exposing (..)

good a =
    case ( 1, 2 ) of
        ( a, b ) ->
            a
|}
  in
  assert_bool "shadowing a parameter is not a duplicate"
    (Option.is_some (declaration_named module_ "good"))

let suite =
  [
    "kernel reference becomes a kernel node"
    >:: test_kernel_reference_becomes_a_kernel_node;
    "kernel without a signature is reported"
    >:: test_kernel_without_a_signature_is_reported;
    "kernel arity must match the signature"
    >:: test_kernel_arity_must_match_the_signature;
    "unknown kernel reference is reported"
    >:: test_unknown_kernel_reference_is_reported;
    "dependency order" >:: test_dependency_order;
    "import cycle" >:: test_import_cycle;
    "alias is the real module" >:: test_alias_is_the_real_module;
    "exposed name becomes global" >:: test_exposed_name_becomes_global;
    "exposing (..) uses dependency exports"
    >:: test_exposing_all_uses_dependency_exports;
    "local declaration shadows import"
    >:: test_local_declaration_shadows_import;
    "parameter shadows import" >:: test_parameter_shadows_import;
    "operators stay local" >:: test_operators_stay_local;
    "exposed ctor in pattern" >:: test_exposed_ctor_in_pattern;
    "qualified type in signature" >:: test_qualified_type_in_signature;
    "unknown module" >:: test_unknown_module;
    "unknown imported module" >:: test_unknown_imported_module;
    "not exposed" >:: test_not_exposed;
    "import of hidden name" >:: test_import_of_hidden_name;
    "ambiguous unqualified" >:: test_ambiguous_unqualified;
    "duplicate constructor" >:: test_duplicate_constructor;
    "duplicate top level declaration" >:: test_duplicate_top_level_declaration;
    "an annotated duplicate is still a duplicate"
    >:: test_an_annotated_duplicate_is_still_a_duplicate;
    "declarations keep their source order"
    >:: test_declarations_keep_their_source_order;
    "constructors not exposed" >:: test_ctors_not_exposed;
    "let binding shadows import" >:: test_let_binding_shadows_import;
    "pattern binding shadows import" >:: test_pattern_binding_shadows_import;
    "alias hides the full module name"
    >:: test_alias_hides_the_full_module_name;
    "type and alias share a name" >:: test_type_and_alias_share_a_name;
    "unqualified use without exposing is unbound"
    >:: test_unqualified_use_without_exposing_is_unbound;
    "diamond dependency order" >:: test_diamond_dependency_order;
    "unknown import is not an edge" >:: test_unknown_import_is_not_an_edge;
    "a_name_bound_twice_in_one_pattern_is_rejected"
    >:: test_a_name_bound_twice_in_one_pattern_is_rejected;
    "a_parameter_bound_twice_is_rejected"
    >:: test_a_parameter_bound_twice_is_rejected;
    "a_lambda_parameter_bound_twice_is_rejected"
    >:: test_a_lambda_parameter_bound_twice_is_rejected;
    "a_binder_may_shadow_an_outer_name"
    >:: test_a_binder_may_shadow_an_outer_name;
  ]
