open OUnit2
open Ast.Kind.Frontend
module Main = Parse.Main

let parse_exn input = match Main.parse input with Ok r -> r | Error e -> raise e

let exposed_summary = function
  | Exposing.Lower n -> "lower:" ^ Data.Located.unwrap n
  | Exposing.Upper { Exposing.name; privacy = Exposing.Private } ->
      "upper:" ^ Data.Located.unwrap name
  | Exposing.Upper { Exposing.name; privacy = Exposing.Public _ } ->
      "upper(..):" ^ Data.Located.unwrap name

let exposing_summary = function
  | Exposing.Open -> ".."
  | Exposing.Explicit [] -> "()"
  | Exposing.Explicit items ->
      String.concat "," (List.map exposed_summary items)

let import_summary (i : Import_thing.t) =
  Printf.sprintf "%s|%s|%s"
    (Data.Located.unwrap i.Import_thing.name)
    (Option.value ~default:"-" i.alias)
    (exposing_summary i.exposing)

let imports_of result =
  List.filter_map (function Impl.Import i -> Some i | _ -> None) result

let decl_expr result name =
  List.find_map
    (function
      | Impl.Top_declaration (d : Declaration.t)
        when Data.Located.unwrap d.body_part.name = name ->
          Some (Data.Located.unwrap d.body_part.expr)
      | _ -> None)
    result

let assert_imports ~expected input =
  let got = List.map import_summary (imports_of (parse_exn input)) in
  assert_equal
    ~printer:(fun l -> "[" ^ String.concat "; " l ^ "]")
    expected got

let test_import_forms _ =
  assert_imports
    ~expected:
      [
        "A|-|()"; "B|Bee|()"; "C|-|..";
        "D|-|lower:foo,upper:Bar,upper(..):Baz";
      ]
    {|
module Main exposing (..)

import A
import B as Bee
import C exposing (..)
import D exposing (foo, Bar, Baz(..))
|}

let test_imports_with_decls _ =
  let input =
    {|
module Main exposing (..)

import Data.List as L exposing (map)

x = 1
|}
  in
  assert_imports ~expected:[ "Data.List|L|lower:map" ] input;
  match decl_expr (parse_exn input) "x" with
  | Some (Expr.Expr_int 1) -> ()
  | _ -> assert_failure "declaration after imports did not parse"

let test_multiline_exposing _ =
  assert_imports
    ~expected:[ "A|-|lower:foo,lower:bar"; "B|-|()" ]
    {|
module Main exposing (..)

import A exposing
    ( foo
    , bar
    )
import B
|}

let test_imports_without_module_header _ =
  assert_imports ~expected:[ "A|-|()" ] {|
import A

x = 1
|}

let assert_expr ~name ~input ~check =
  match decl_expr (parse_exn input) name with
  | Some e -> check e
  | None -> assert_failure (Printf.sprintf "declaration %s not found" name)

let test_qualified_refs _ =
  let input =
    {|
module Main exposing (..)

a = A.foo
b = A.Ctor
c = A.B.foo
|}
  in
  assert_expr ~name:"a" ~input ~check:(function
    | Expr.Expr_qualified { qualifier = "A"; name = "foo" } -> ()
    | e -> assert_failure ("a: " ^ Expr.show e));
  assert_expr ~name:"b" ~input ~check:(function
    | Expr.Expr_qualified { qualifier = "A"; name = "Ctor" } -> ()
    | e -> assert_failure ("b: " ^ Expr.show e));
  assert_expr ~name:"c" ~input ~check:(function
    | Expr.Expr_qualified { qualifier = "A.B"; name = "foo" } -> ()
    | e -> assert_failure ("c: " ^ Expr.show e))

let test_qualified_then_record_access _ =
  assert_expr ~name:"d"
    ~input:{|
module Main exposing (..)

d = A.foo.bar
|}
    ~check:(function
      | Expr.Expr_access
          { expr = Expr.Expr_qualified { qualifier = "A"; name = "foo" }; field }
        ->
          assert_equal ~printer:Fun.id "bar" (Data.Located.unwrap field)
      | e -> assert_failure ("d: " ^ Expr.show e))

let test_plain_record_access_unaffected _ =
  assert_expr ~name:"e"
    ~input:{|
module Main exposing (..)

e = r.foo
|}
    ~check:(function
      | Expr.Expr_access { expr = Expr.Expr_ident "r"; field } ->
          assert_equal ~printer:Fun.id "foo" (Data.Located.unwrap field)
      | e -> assert_failure ("e: " ^ Expr.show e))

let suite =
  [
    "test_import_forms" >:: test_import_forms;
    "test_imports_with_decls" >:: test_imports_with_decls;
    "test_multiline_exposing" >:: test_multiline_exposing;
    "test_imports_without_module_header" >:: test_imports_without_module_header;
    "test_qualified_refs" >:: test_qualified_refs;
    "test_qualified_then_record_access" >:: test_qualified_then_record_access;
    "test_plain_record_access_unaffected" >:: test_plain_record_access_unaffected;
  ]
