open OUnit2

let declaration_named module_ name =
  List.find_opt
    (fun (d : Canonical.Declaration.t) ->
      String.equal (Data.Located.unwrap d.body_part.name) name)
    module_.Canonical.Module.top_declarations

let declaration module_ name =
  declaration_named module_ name
  |> Option.map (fun (d : Canonical.Declaration.t) ->
         Data.Located.unwrap d.body_part.expr)

let expr_of module_ name =
  match declaration module_ name with
  | Some expr -> expr
  | None -> assert_failure (Printf.sprintf "declaration %s not found" name)

let test_module_name_and_exports _ =
  let module_ =
    Utils.canonical {|
module Main exposing (x, User, Color(..))

x = 1
|}
  in
  assert_equal ~printer:Fun.id "Main" module_.Canonical.Module.name;
  assert_equal
    ~printer:Canonical.Exposed.show
    (Canonical.Exposed.Only
       [
         Canonical.Exposed.Value "x";
         Canonical.Exposed.Type { name = "User"; ctors_exposed = false };
         Canonical.Exposed.Type { name = "Color"; ctors_exposed = true };
       ])
    module_.Canonical.Module.exports

let test_open_exports _ =
  let module_ = Utils.canonical {|
module Main exposing (..)

x = 1
|} in
  assert_equal ~printer:Canonical.Exposed.show Canonical.Exposed.All
    module_.Canonical.Module.exports

let test_imports_are_carried _ =
  let module_ =
    Utils.canonical
      {|
module Main exposing (..)

import Data.List as L exposing (map, Set(..))
import Plain

x = 1
|}
  in
  assert_equal
    ~printer:(fun imports ->
      String.concat "; "
        (List.map
           (fun (import : Canonical.Import.t) ->
             Canonical.Import.show { import with region = Data.Region.nowhere })
           imports))
    ~cmp:(fun expected actual ->
      List.equal
        (fun (one : Canonical.Import.t) (other : Canonical.Import.t) ->
          { one with region = Data.Region.nowhere }
          = { other with region = Data.Region.nowhere })
        expected actual)
    [
      {
        Canonical.Import.module_name = "Data.List";
        alias = Some "L";
        exposed =
          Canonical.Exposed.Only
            [
              Canonical.Exposed.Value "map";
              Canonical.Exposed.Type { name = "Set"; ctors_exposed = true };
            ];
        region = Data.Region.nowhere;
      };
      {
        Canonical.Import.module_name = "Plain";
        alias = None;
        exposed = Canonical.Exposed.Only [];
        region = Data.Region.nowhere;
      };
    ]
    module_.Canonical.Module.imports

let test_local_names_stay_local _ =
  let module_ = Utils.canonical {|
module Main exposing (..)

x = y
|} in
  match expr_of module_ "x" with
  | Canonical.Expr.Expr_ident (Data.Name.Local "y") -> ()
  | e -> assert_failure ("x: " ^ Canonical.Expr.show_expr e)

let test_qualified_value_becomes_global _ =
  let module_ =
    Utils.canonical {|
module Main exposing (..)

x = A.foo
y = Data.List.map
|}
  in
  (match expr_of module_ "x" with
  | Canonical.Expr.Expr_ident
      (Data.Name.Global { module_name = "A"; exported_name = "foo" }) ->
      ()
  | e -> assert_failure ("x: " ^ Canonical.Expr.show_expr e));
  match expr_of module_ "y" with
  | Canonical.Expr.Expr_ident
      (Data.Name.Global { module_name = "Data.List"; exported_name = "map" }) ->
      ()
  | e -> assert_failure ("y: " ^ Canonical.Expr.show_expr e)

let test_qualified_constructor_becomes_global _ =
  let module_ = Utils.canonical {|
module Main exposing (..)

x = A.Ctor
|} in
  match expr_of module_ "x" with
  | Canonical.Expr.Expr_ident
      (Data.Name.Global { module_name = "A"; exported_name = "Ctor" }) ->
      ()
  | e -> assert_failure ("x: " ^ Canonical.Expr.show_expr e)

let test_qualified_type_becomes_global _ =
  let module_ =
    Utils.canonical {|
module Main exposing (..)

x : A.Thing
x = 1
|}
  in
  match declaration_named module_ "x" with
  | Some { type_part_data = Some { type_alias; _ }; _ } -> (
      match type_alias.Canonical.Typedef.Impl.body with
      | Canonical.Typedef.Kind.Tkind_concrete name -> (
          match Data.Located.unwrap name with
          | Data.Name.Global { module_name = "A"; exported_name = "Thing" } ->
              ()
          | other -> assert_failure ("type: " ^ Data.Name.show other))
      | _ -> assert_failure "signature is not a concrete type")
  | _ -> assert_failure "type signature not canonicalized"

let test_unit_and_negation _ =
  let module_ =
    Utils.canonical {|
module Main exposing (..)

nothing = ()
negated = -x
|}
  in
  (match expr_of module_ "nothing" with
  | Canonical.Expr.Expr_unit -> ()
  | e -> assert_failure ("nothing: " ^ Canonical.Expr.show_expr e));
  match expr_of module_ "negated" with
  | Canonical.Expr.Expr_apply
      {
        fn = { thing = Canonical.Expr.Expr_ident (Data.Name.Local "negate"); _ };
        arg = { thing = Canonical.Expr.Expr_ident (Data.Name.Local "x"); _ };
      } ->
      ()
  | e -> assert_failure ("negated: " ^ Canonical.Expr.show_expr e)

let suite =
  [
    "test_module_name_and_exports" >:: test_module_name_and_exports;
    "test_open_exports" >:: test_open_exports;
    "test_imports_are_carried" >:: test_imports_are_carried;
    "test_local_names_stay_local" >:: test_local_names_stay_local;
    "test_qualified_value_becomes_global" >:: test_qualified_value_becomes_global;
    "test_qualified_constructor_becomes_global"
    >:: test_qualified_constructor_becomes_global;
    "test_qualified_type_becomes_global" >:: test_qualified_type_becomes_global;
    "test_unit_and_negation" >:: test_unit_and_negation;
  ]
