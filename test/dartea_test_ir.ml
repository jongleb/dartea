open OUnit2
open Ir.Ast
module O = Optimized
module Text = Set.Make (String)
module Labels = Map.Make (String)

let scope_problems (declaration : declaration) =
  let found = ref [] in
  let report message =
    found := Printf.sprintf "%s: %s" declaration.name message :: !found
  in
  let atom ~bound = function
    | A_var name when not (Text.mem name bound) ->
        report ("free variable " ^ name)
    | A_var _ | A_int _ | A_float _ | A_string _ | A_char _ | A_unit | A_nil
    | A_constant _ | A_global _ ->
        ()
  in
  let rec bind ~bound = function
    | B_atom value -> atom ~bound value
    | B_construct { arguments; _ }
    | B_call { arguments; _ }
    | B_primitive { arguments; _ }
    | B_kernel { arguments; _ }
    | B_partial { arguments; _ } ->
        List.iter (atom ~bound) arguments
    | B_tuple { items } -> List.iter (atom ~bound) items
    | B_cons { head; tail } ->
        atom ~bound head;
        atom ~bound tail
    | B_record { fields } ->
        List.iter (fun (_, value) -> atom ~bound value) fields
    | B_record_update { base; fields } ->
        atom ~bound base;
        List.iter (fun (_, value) -> atom ~bound value) fields
    | B_access { subject; _ } -> atom ~bound subject
    | B_call_closure { callee; arguments } ->
        atom ~bound callee;
        List.iter (atom ~bound) arguments
    | B_closure { parameters; captures; body } ->
        List.iter (fun name -> atom ~bound (A_var name)) captures;
        term
          ~bound:
            (List.fold_left
               (fun known name -> Text.add name known)
               (Text.of_list captures) parameters)
          ~labels:Labels.empty body
  and term ~bound ~labels = function
    | T_return value -> atom ~bound value
    | T_let { name; bind = bound_value; body } ->
        bind ~bound bound_value;
        term ~bound:(Text.add name bound) ~labels body
    | T_if { condition; consequent; alternative } ->
        atom ~bound condition;
        term ~bound ~labels consequent;
        term ~bound ~labels alternative
    | T_switch { subject; branches; default } ->
        atom ~bound subject;
        List.iter (fun (_, child) -> term ~bound ~labels child) branches;
        Option.iter (term ~bound ~labels) default
    | T_join { label; parameters; definition; body } ->
        term
          ~bound:
            (List.fold_left
               (fun known name -> Text.add name known)
               bound parameters)
          ~labels definition;
        term ~bound
          ~labels:(Labels.add label (List.length parameters) labels)
          body
    | T_jump { label; arguments } -> begin
        List.iter (atom ~bound) arguments;
        match Labels.find_opt label labels with
        | None -> report ("jump to an undeclared label " ^ label)
        | Some arity when arity <> List.length arguments ->
            report ("jump to " ^ label ^ " with the wrong argument count")
        | Some _ -> ()
      end
    | T_fail _ -> ()
  in
  term
    ~bound:(Text.of_list declaration.parameters)
    ~labels:Labels.empty declaration.body;
  List.rev !found

let generated_names (declaration : declaration) =
  let found = ref [] in
  let keep name =
    if String.starts_with ~prefix:"#" name then found := name :: !found
  in
  let rec bind = function
    | B_closure { parameters; body; _ } ->
        List.iter keep parameters;
        term body
    | B_atom _ | B_construct _ | B_cons _ | B_tuple _ | B_record _
    | B_record_update _ | B_access _ | B_call _ | B_call_closure _
    | B_partial _ | B_primitive _ | B_kernel _ ->
        ()
  and term = function
    | T_return _ | T_jump _ | T_fail _ -> ()
    | T_let { name; bind = bound_value; body } ->
        keep name;
        bind bound_value;
        term body
    | T_if { consequent; alternative; _ } ->
        term consequent;
        term alternative
    | T_switch { branches; default; _ } ->
        List.iter (fun (_, child) -> term child) branches;
        Option.iter term default
    | T_join { label; parameters; definition; body } ->
        keep label;
        List.iter keep parameters;
        term definition;
        term body
  in
  List.iter keep declaration.parameters;
  term declaration.body;
  List.rev !found

module Ir_backend : Dartea.Compiler.BACKEND = struct
  let extension = "ir"
  let runtime_modules _ = []
  let platform_kernel _ = None
  let through _ = None

  let emit_module ~blocks:_ ~arities ~constructors ~built:_ ~siblings
      ~typedecls:_ ~imports:_ ~exports:_ declarations =
    ( Ir.Of_optimized.convert ~arities ~constructors ~siblings declarations
      |> Ir.To_string.program,
      [] )
end

module Checked_backend : Dartea.Compiler.BACKEND = struct
  let extension = "problems"
  let platform_kernel _ = None
  let through _ = None
  let runtime_modules _ = []

  let emit_module ~blocks:_ ~arities ~constructors ~built:_ ~siblings
      ~typedecls:_ ~imports:_ ~exports:_ declarations =
    ( Ir.Of_optimized.convert ~arities ~constructors ~siblings declarations
      |> List.concat_map scope_problems
      |> String.concat "\n",
      [] )
end

module Compiler = Dartea.Compiler.Make (Ir_backend)
module Checker = Dartea.Compiler.Make (Checked_backend)

let test_a_backend_without_the_platform_refuses_it _ =
  let outcome =
    Compiler.compile_modules ~entry:None
      (Project.Sources.of_list
         [
           Project.Elm_file.of_path ~path:"Main.elm"
             "module Main exposing (hello)\n\nimport VirtualDom\n\nhello :            VirtualDom.Node msg\nhello = VirtualDom.text \"hi\"\n";
         ])
  in
  match outcome.errors with
  | { problem = Syntax _ | Type _ | Project _; _ } :: _ | [] ->
      assert_failure "the platform kernel was accepted without a platform"
  | { problem = Name (Unknown_kernel { module_name; _ }); _ } :: _ ->
      assert_bool module_name
        (String.starts_with ~prefix:"Elm.Kernel." module_name)
  | { problem = Name _; _ } :: _ ->
      assert_failure "a different naming error came out"

let lowered source =
  let outcome = Compiler.compile_source source in
  match outcome.errors with
  | [] -> Compiler.link ~roots:(Dartea.Compiler.everything outcome) outcome
  | error :: _ -> raise (Reporting.Error.Found error)

let ir_of source =
  lowered source
  |> List.filter_map (fun (unit : Dartea.Compiler.artifact) ->
         if String.equal unit.module_name "Main" then Some unit.source else None)
  |> String.concat ""

let well_scoped source =
  let outcome = Checker.compile_source source in
  begin
    match outcome.errors with
    | [] -> Checker.link ~roots:(Dartea.Compiler.everything outcome) outcome
    | error :: _ -> raise (Reporting.Error.Found error)
  end
  |> List.concat_map (fun (unit : Dartea.Compiler.artifact) ->
         if String.equal unit.source "" then []
         else [ unit.module_name ^ " -> " ^ unit.source ])
  |> String.concat "\n"

let assert_scoped source _ = assert_equal ~printer:Fun.id "" (well_scoped source)

let assert_ir ~expected source _ =
  assert_equal ~printer:Fun.id (String.trim expected) (String.trim (ir_of source))

let adt_source =
  {elm|module Main exposing (area)

type Shape
    = Circle Int
    | Rect Int Int

area shape =
    case shape of
        Circle r -> r * r
        Rect w h -> w * h
|elm}

let record_source =
  {elm|module Main exposing (label)

label person =
    person.name ++ "!"
|elm}

let list_source =
  {elm|module Main exposing (numbers)

numbers =
    [ 1, 2, 3 ]
|elm}

let higher_order_source =
  {elm|module Main exposing (twice)

twice f n =
    f (f n)
|elm}

let capture_source =
  {elm|module Main exposing (boxed)

boxed n =
    Just (\x -> x + n)
|elm}

let nested_source =
  {elm|module Main exposing (describe)

describe flag =
    let
        value =
            if flag then String.length "yes" else 0
    in
    value + 1
|elm}

let adt_ir =
  {ir|area(shape):
  switch shape
    tag Circle:
      let #f1 = payload 0 of shape
      let r = #f1
      let #v2 = primitive *(r, r)
      return #v2
    tag Rect:
      let #f3 = payload 0 of shape
      let w = #f3
      let #f4 = payload 1 of shape
      let h = #f4
      let #v5 = primitive *(w, h)
      return #v5|ir}

let record_ir =
  {ir|label(person):
  let #a1 = field name of person
  let #v2 = primitive ++(#a1, "!")
  return #v2|ir}

let list_ir =
  {ir|numbers():
  let #l1 = cons 3 []
  let #l2 = cons 2 #l1
  let #v3 = cons 1 #l2
  return #v3|ir}

let higher_order_ir =
  {ir|twice(f, n):
  let #a1 = call_closure f(n)
  let #v2 = call_closure f(#a1)
  return #v2|ir}

let capture_ir =
  {ir|boxed(n):
  let #a1 = closure (x) capturing (n)
    let #v3 = primitive +(x, n)
    return #v3
  let #v2 = call_closure @Maybe.Just(#a1)
  return #v2|ir}

let nested_ir =
  {ir|describe(flag):
  join #join2(value)
    let #v1 = primitive +(value, 1)
    return #v1
  if flag
    let #v3 = call String.length("yes")
    jump #join2(#v3)
  else
    jump #join2(0)|ir}

open QCheck2

let typ = O.Type.TUnit
let node expr : O.Expr.t = { O.Expr.typ; expr }
let name_gen = Gen.oneof_list [ "a"; "b"; "x"; "value" ]
let constructors = [ ("Nought", 0); ("One", 1); ("Two", 2) ]

let constructor_arities =
  List.map (fun (name, arity) -> (Data.Name.local name, arity)) constructors

let sibling_env =
  List.map
    (fun (name, _) -> (Data.Name.local name, constructor_arities))
    constructors

let constructor_gen = Gen.oneof_list constructors

let pattern name = { O.Pattern.typ; pattern = O.Pattern.P_T_var name }
let wildcard = { O.Pattern.typ; pattern = O.Pattern.P_T_anything }

type flavour = Integers | Constructors | Lists | Names

let patterns_gen =
  let of_flavour flavour =
    match flavour with
    | Integers ->
        Gen.list_size (Gen.int_range 1 3)
          (Gen.map
             (fun value -> { O.Pattern.typ; pattern = O.Pattern.P_T_int value })
             (Gen.int_range 0 3))
    | Constructors ->
        Gen.list_size (Gen.int_range 1 3)
          (Gen.map2
             (fun (name, arity) bound ->
               {
                 O.Pattern.typ;
                 pattern =
                   O.Pattern.P_T_ctor
                     ( Data.Name.local name,
                       List.init arity (fun index ->
                           pattern (bound ^ string_of_int index)) );
               })
             constructor_gen name_gen)
    | Lists ->
        Gen.list_size (Gen.int_range 1 2)
          (Gen.map2
             (fun head tail ->
               {
                 O.Pattern.typ;
                 pattern = O.Pattern.P_T_cons (pattern head, pattern tail);
               })
             name_gen name_gen)
    | Names -> Gen.map (fun name -> [ pattern name ]) name_gen
  in
  Gen.bind
    (Gen.oneof_list [ Integers; Constructors; Lists; Names ])
    (fun flavour ->
      Gen.map (fun patterns -> patterns @ [ wildcard ]) (of_flavour flavour))

let rec expr_gen depth =
  let leaf =
    Gen.oneof
      [
        Gen.map (fun value -> node (O.Expr.Expr_int value)) (Gen.int_range 0 9);
        Gen.map
          (fun value -> node (O.Expr.Expr_string value))
          (Gen.oneof_list [ "hi"; "" ]);
        Gen.map
          (fun name -> node (O.Expr.Expr_ident (Data.Name.local name)))
          name_gen;
        Gen.return (node O.Expr.Expr_unit);
      ]
  in
  if depth <= 0 then leaf
  else
    let smaller = expr_gen (depth - 1) in
    Gen.oneof_weighted
      [
        (4, leaf);
        ( 2,
          Gen.map3
            (fun name bound body ->
              node
                (O.Expr.Expr_let
                   {
                     binding =
                       { bind_body = { name = Data.Located.dummy name; body = bound } };
                     body;
                   }))
            name_gen smaller smaller );
        ( 2,
          Gen.map3
            (fun if_exp then_exp else_exp ->
              node (O.Expr.Expr_if_then_else { if_exp; then_exp; else_exp }))
            smaller smaller smaller );
        (2, Gen.map2 (fun fn arg -> node (O.Expr.Expr_apply { fn; arg })) smaller smaller);
        ( 1,
          Gen.map2
            (fun parameters body ->
              node
                (O.Expr.Expr_lambda
                   {
                     params =
                       List.map
                         (fun name ->
                           { O.Expr.name = Data.Located.dummy name; typ })
                         parameters;
                     body;
                   }))
            (Gen.list_size (Gen.int_range 1 2) name_gen)
            smaller );
        ( 1,
          Gen.map
            (fun items -> node (O.Expr.Expr_list items))
            (Gen.list_size (Gen.int_range 0 3) smaller) );
        ( 1,
          Gen.bind constructor_gen (fun (name, arity) ->
              Gen.map
                (fun arguments ->
                  node
                    (O.Expr.Expr_constr
                       { name = Data.Name.local name; arguments }))
                (Gen.list_size (Gen.return arity) smaller)) );
        ( 1,
          Gen.map
            (fun fields ->
              node
                (O.Expr.Expr_record
                   (List.map (fun (name, value) -> { O.Expr.name; value }) fields)))
            (Gen.list_size (Gen.int_range 1 3) (Gen.pair name_gen smaller)) );
        ( 1,
          Gen.map2
            (fun expr field ->
              node
                (O.Expr.Expr_access { expr; field = Data.Located.dummy field }))
            smaller name_gen );
        ( 2,
          Gen.map3
            (fun expr patterns bodies ->
              node
                (O.Expr.Expr_pattern
                   {
                     expr;
                     pattern_data_items =
                       List.mapi
                         (fun index pattern ->
                           {
                             O.Expr.pattern;
                             expr =
                               Option.value
                                 (List.nth_opt bodies index)
                                 ~default:(node (O.Expr.Expr_int index));
                           })
                         patterns;
                   }))
            smaller patterns_gen
            (Gen.list_size (Gen.int_range 1 4) smaller) );
      ]

let declaration_gen =
  Gen.map3
    (fun name parameters body ->
      {
        O.Declaration.name = Data.Located.dummy name;
        params =
          List.map
            (fun parameter ->
              { O.Declaration.name = Data.Located.dummy parameter; typ })
            parameters;
        body;
        typ;
      })
    name_gen
    (Gen.list_size (Gen.int_range 0 2) name_gen)
    (expr_gen 3)

let converted declaration =
  Ir.Of_optimized.convert ~arities:[] ~constructors:constructor_arities
    ~siblings:sibling_env [ declaration ]

let show_declaration = O.Declaration.show

let law_well_scoped =
  Test.make ~count:1000 ~name:"every variable the IR mentions is bound"
    ~print:show_declaration declaration_gen (fun declaration ->
      List.concat_map scope_problems (converted declaration) = [])

let law_generated_names_are_unique =
  Test.make ~count:1000 ~name:"generated names are unique inside a declaration"
    ~print:show_declaration declaration_gen (fun declaration ->
      List.for_all
        (fun value ->
          let names = generated_names value in
          List.length (List.sort_uniq String.compare names) = List.length names)
        (converted declaration))

let law_conversion_is_deterministic =
  Test.make ~count:500 ~name:"conversion of the same declaration is stable"
    ~print:show_declaration declaration_gen (fun declaration ->
      String.equal
        (Ir.To_string.program (converted declaration))
        (Ir.To_string.program (converted declaration)))

let suite =
  [
    "a backend without the platform refuses its kernels"
    >:: test_a_backend_without_the_platform_refuses_it;
    "adt is well scoped" >:: assert_scoped adt_source;
    "record is well scoped" >:: assert_scoped record_source;
    "list is well scoped" >:: assert_scoped list_source;
    "higher order is well scoped" >:: assert_scoped higher_order_source;
    "capture is well scoped" >:: assert_scoped capture_source;
    "nested is well scoped" >:: assert_scoped nested_source;
    "adt lowers to a switch on the tag" >:: assert_ir ~expected:adt_ir adt_source;
    "record access lowers to a field read"
    >:: assert_ir ~expected:record_ir record_source;
    "a list literal lowers to a chain of conses"
    >:: assert_ir ~expected:list_ir list_source;
    "an unknown callee lowers to a closure call"
    >:: assert_ir ~expected:higher_order_ir higher_order_source;
    "a lambda lowers to a closure with captures"
    >:: assert_ir ~expected:capture_ir capture_source;
    "a branching let lowers to a join point"
    >:: assert_ir ~expected:nested_ir nested_source;
  ]
  @ QCheck_ounit.to_ounit2_test_list
      [
        law_well_scoped; law_generated_names_are_unique;
        law_conversion_is_deterministic;
      ]
