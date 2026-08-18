open OUnit2
module Scope = Canonicalization.Scope

let canonical input =
  match Parse.Main.parse ~file:"Main.elm" input with
  | Error error -> raise (Reporting.Error.Found error)
  | Ok impl_list ->
      Canonical.Module.of_frontend ~fallback_name:"Main"
        (Ast.Kind.Frontend.Module.of_impl impl_list)

let declaration source name =
  match
    List.find_opt
      (fun (d : Canonical.Declaration.t) ->
        String.equal (Data.Located.unwrap d.body_part.name) name)
      (canonical source).Canonical.Module.top_declarations
  with
  | Some declaration -> declaration
  | None -> assert_failure (Printf.sprintf "no declaration named %s" name)

let free source name =
  Scope.free_in_declaration (declaration source name)
  |> Scope.Names.elements
  |> List.map Data.Name.to_string

let assert_free ~src ~name ~expected =
  assert_equal
    ~printer:(fun names -> String.concat ", " names)
    expected (free src name)

let test_a_parameter_shadows_the_top_level _ =
  assert_free ~src:{|
value = 1

shadowing value = value
|} ~name:"shadowing"
    ~expected:[]

let test_an_unbound_reference_is_free _ =
  assert_free ~src:{|
value = 1

using = value
|} ~name:"using"
    ~expected:[ "value" ]

let test_a_let_binder_scopes_over_its_own_right_hand_side _ =
  assert_free
    ~src:{|
counting =
    let
        x = x + 1
    in
    x
|}
    ~name:"counting" ~expected:[ "+" ]

let test_a_let_binder_scopes_over_the_body _ =
  assert_free
    ~src:{|
counting =
    let
        x = 1
    in
    x + total
|}
    ~name:"counting" ~expected:[ "+"; "total" ]

let test_a_pattern_binder_shadows_the_top_level _ =
  assert_free
    ~src:
      {|
value = 1

matching subject =
    case subject of
        Box value -> value
|}
    ~name:"matching" ~expected:[ "Box" ]

let test_a_constructor_in_a_pattern_is_a_mention _ =
  assert_free
    ~src:
      {|
describing subject =
    case subject of
        Nothing -> zero
        Just payload -> payload
|}
    ~name:"describing" ~expected:[ "Just"; "Nothing"; "zero" ]

let test_a_constructor_in_an_expression_is_a_mention _ =
  assert_free ~src:{|
wrapping payload = Just payload
|} ~name:"wrapping"
    ~expected:[ "Just" ]

let test_nested_lambdas_bind_their_parameters _ =
  assert_free
    ~src:{|
composing = \f -> \g -> \x -> f (g x) (h x)
|}
    ~name:"composing" ~expected:[ "h" ]

let test_a_lambda_parameter_shadows_the_top_level _ =
  assert_free ~src:{|
f x = 1

using = \f -> f 1
|} ~name:"using"
    ~expected:[]

let test_a_qualified_reference_is_free _ =
  assert_free ~src:{|
size text = String.length text
|} ~name:"size"
    ~expected:[ "String.length" ]

let test_a_recursive_declaration_mentions_itself _ =
  assert_free
    ~src:{|
countdown n = if n == 0 then 0 else countdown (n - 1)
|}
    ~name:"countdown" ~expected:[ "-"; "=="; "countdown" ]

let test_an_annotation_contributes_no_free_names _ =
  assert_free ~src:{|
label : Color -> String
label color = name color
|} ~name:"label"
    ~expected:[ "name" ]

let suite =
  [
    "a_parameter_shadows_the_top_level" >:: test_a_parameter_shadows_the_top_level;
    "an_unbound_reference_is_free" >:: test_an_unbound_reference_is_free;
    "a_let_binder_scopes_over_its_own_right_hand_side"
    >:: test_a_let_binder_scopes_over_its_own_right_hand_side;
    "a_let_binder_scopes_over_the_body"
    >:: test_a_let_binder_scopes_over_the_body;
    "a_pattern_binder_shadows_the_top_level"
    >:: test_a_pattern_binder_shadows_the_top_level;
    "a_constructor_in_a_pattern_is_a_mention"
    >:: test_a_constructor_in_a_pattern_is_a_mention;
    "a_constructor_in_an_expression_is_a_mention"
    >:: test_a_constructor_in_an_expression_is_a_mention;
    "nested_lambdas_bind_their_parameters"
    >:: test_nested_lambdas_bind_their_parameters;
    "a_lambda_parameter_shadows_the_top_level"
    >:: test_a_lambda_parameter_shadows_the_top_level;
    "a_qualified_reference_is_free" >:: test_a_qualified_reference_is_free;
    "a_recursive_declaration_mentions_itself"
    >:: test_a_recursive_declaration_mentions_itself;
    "an_annotation_contributes_no_free_names"
    >:: test_an_annotation_contributes_no_free_names;
  ]
