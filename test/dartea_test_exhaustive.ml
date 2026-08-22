open OUnit2
module E = After_typed.Exhaustive
module P = Optimized.Pattern
module M = Data.Name.Map

let mk k : P.t = { P.typ = Optimized.Type.TUnit; pattern = k }
let anything = mk P.P_T_anything
let var x = mk (P.P_T_var x)
let unit = mk P.P_T_unit
let ctor n args = mk (P.P_T_ctor (Data.Name.local n, args))
let int n = mk (P.P_T_int n)
let str s = mk (P.P_T_str s)
let chr c = mk (P.P_T_chr c)
let record fields = mk (P.P_T_record fields)
let nil = mk (P.P_T_list [])
let cons h t = mk (P.P_T_cons (h, t))
let list xs = mk (P.P_T_list xs)
let tuple ps = mk (P.P_T_tuple ps)
let alias p name = mk (P.P_T_alias (p, name))
let mkenv pairs =
  List.fold_left (fun m (k, v) -> M.add (Data.Name.local k) v m) M.empty pairs

let variant ctors =
  let siblings = List.map (fun (name, arity) -> (Data.Name.local name, arity)) ctors in
  mkenv (List.map (fun (name, _) -> (name, siblings)) ctors)
let merge = M.union (fun _ a _ -> Some a)

let assert_exhaustive ?(env = M.empty) pats =
  assert_bool "expected exhaustive" (E.is_exhaustive env pats)

let assert_not_exhaustive ?(env = M.empty) pats =
  assert_bool "expected non-exhaustive" (not (E.is_exhaustive env pats))

let color = variant [ ("Red", 0); ("Green", 0); ("Blue", 0) ]
let inner = variant [ ("C", 0); ("D", 0) ]
let outer_inner = merge (variant [ ("E", 1); ("F", 1) ]) inner

let test_enum_complete _ =
  assert_exhaustive ~env:color
    [ ctor "Red" []; ctor "Green" []; ctor "Blue" [] ]

let test_enum_missing _ =
  assert_not_exhaustive ~env:color [ ctor "Red" []; ctor "Green" [] ]

let test_enum_wildcard _ =
  assert_exhaustive ~env:color [ ctor "Red" []; anything ]

let test_enum_redundant_still_exhaustive _ =
  assert_exhaustive ~env:color
    [ ctor "Red" []; ctor "Green" []; ctor "Blue" []; anything ]

let test_wildcard_first_is_exhaustive _ =
  assert_exhaustive ~env:color [ anything; ctor "Red" [] ]

let test_bool_unregistered_is_incomplete _ =
  assert_not_exhaustive [ ctor "True" []; ctor "False" [] ]

let test_bool_as_variant_is_complete _ =
  assert_exhaustive
    ~env:(variant [ ("True", 0); ("False", 0) ])
    [ ctor "True" []; ctor "False" [] ]

let test_nested_complete _ =
  assert_exhaustive ~env:outer_inner
    [
      ctor "E" [ ctor "C" [] ];
      ctor "E" [ ctor "D" [] ];
      ctor "F" [ ctor "C" [] ];
      ctor "F" [ ctor "D" [] ];
    ]

let test_nested_missing _ =
  assert_not_exhaustive ~env:outer_inner
    [
      ctor "E" [ ctor "C" [] ];
      ctor "E" [ ctor "D" [] ];
      ctor "F" [ ctor "C" [] ];
    ]

let test_nested_inner_wildcard _ =
  assert_exhaustive ~env:outer_inner
    [
      ctor "E" [ ctor "C" [] ]; ctor "E" [ ctor "D" [] ]; ctor "F" [ anything ];
    ]

let test_payload_wildcards _ =
  assert_exhaustive ~env:outer_inner
    [ ctor "E" [ anything ]; ctor "F" [ var "x" ] ]

let test_list_complete _ =
  assert_exhaustive [ nil; cons (var "h") (var "t") ]

let test_list_only_cons _ =
  assert_not_exhaustive [ cons (var "h") (var "t") ]

let test_list_only_nil _ = assert_not_exhaustive [ nil ]
let test_list_nil_wildcard _ = assert_exhaustive [ nil; anything ]

let test_list_nested_cons _ =
  assert_exhaustive
    [
      cons (var "a") (cons (var "b") anything);
      cons (var "a") anything;
      nil;
    ]

let test_int_no_wildcard _ = assert_not_exhaustive [ int 0; int 1 ]
let test_int_with_wildcard _ = assert_exhaustive [ int 0; int 1; anything ]
let test_single_var _ = assert_exhaustive [ var "x" ]

let test_tuple_complete _ =
  assert_exhaustive ~env:inner
    [
      tuple [ ctor "C" []; ctor "C" [] ];
      tuple [ ctor "C" []; ctor "D" [] ];
      tuple [ ctor "D" []; ctor "C" [] ];
      tuple [ ctor "D" []; ctor "D" [] ];
    ]

let test_tuple_missing _ =
  assert_not_exhaustive ~env:inner
    [
      tuple [ ctor "C" []; ctor "C" [] ];
      tuple [ ctor "C" []; ctor "D" [] ];
      tuple [ ctor "D" []; ctor "C" [] ];
    ]

let test_tuple_component_wildcard _ =
  assert_exhaustive ~env:inner
    [ tuple [ ctor "C" []; anything ]; tuple [ ctor "D" []; anything ] ]

let test_redundant_duplicate _ =
  assert_equal ~printer:(fun l -> String.concat "," (List.map string_of_int l))
    [ 1 ]
    (E.redundant_clauses color
       [ ctor "Red" []; ctor "Red" []; ctor "Green" [] ])

let test_redundant_after_wildcard _ =
  assert_equal ~printer:(fun l -> String.concat "," (List.map string_of_int l))
    [ 1 ]
    (E.redundant_clauses color [ anything; ctor "Red" [] ])

let test_no_redundant _ =
  assert_equal ~printer:(fun l -> String.concat "," (List.map string_of_int l))
    []
    (E.redundant_clauses color
       [ ctor "Red" []; ctor "Green" []; ctor "Blue" [] ])

let witness_head env pats =
  match E.counterexample env pats with
  | Some p -> E.head_of p
  | None -> None

let test_witness_list_nil _ =
  assert_equal (Some E.H_cons) (witness_head M.empty [ nil ])

let test_witness_list_cons _ =
  assert_equal (Some E.H_nil) (witness_head M.empty [ cons (var "h") (var "t") ])

let test_witness_exhaustive _ =
  assert_bool "no counterexample when exhaustive"
    (E.counterexample color [ ctor "Red" []; ctor "Green" []; ctor "Blue" [] ] = None)

let test_witness_nonexhaustive _ =
  assert_bool "counterexample present when non-exhaustive"
    (E.counterexample color [ ctor "Red" []; ctor "Green" [] ] <> None)

let test_witness_nested _ =
  assert_equal (Some (E.H_ctor (Data.Name.local "F")))
    (witness_head outer_inner
       [ ctor "E" [ ctor "C" [] ]; ctor "E" [ ctor "D" [] ]; ctor "F" [ ctor "C" [] ] ])

let test_witness_missing_ctor_named _ =
  assert_equal (Some (E.H_ctor (Data.Name.local "Blue")))
    (witness_head color [ ctor "Red" []; ctor "Green" [] ])

let test_witness_int_gap _ =
  assert_equal (Some (E.H_int 1)) (witness_head M.empty [ int 0; int 2 ])

let test_witness_string_gap _ =
  assert_equal (Some (E.H_str "aa")) (witness_head M.empty [ str "a"; str "" ])

let test_witness_char_gap _ =
  assert_equal (Some (E.H_chr "c")) (witness_head M.empty [ chr "a"; chr "b" ])

let test_empty_match _ = assert_not_exhaustive []
let test_unit_exhaustive _ = assert_exhaustive [ unit ]

let test_record_exhaustive _ = assert_exhaustive [ record [ "x"; "y" ] ]

let test_record_before_wildcard_redundant _ =
  assert_equal ~printer:(fun l -> String.concat "," (List.map string_of_int l))
    [ 1 ]
    (E.redundant_clauses M.empty [ record [ "x" ]; anything ])

let test_str_no_wildcard _ = assert_not_exhaustive [ str "a"; str "b" ]
let test_str_with_wildcard _ = assert_exhaustive [ str "a"; anything ]
let test_chr_no_wildcard _ = assert_not_exhaustive [ chr "a"; chr "b" ]
let test_chr_with_wildcard _ = assert_exhaustive [ chr "a"; anything ]

let test_list_literal_missing _ =
  assert_not_exhaustive [ list [ var "a" ] ]

let test_list_literal_with_nil_and_cons _ =
  assert_exhaustive [ nil; cons (var "h") (var "t") ]

let test_record_inside_ctor _ =
  assert_exhaustive ~env:(variant [ ("Box", 1) ]) [ ctor "Box" [ record [ "x" ] ] ]

let test_witness_tuple _ =
  assert_bool "tuple witness is a tuple"
    (match E.counterexample inner [ tuple [ ctor "C" []; ctor "C" [] ] ] with
     | Some p -> (match p.P.pattern with P.P_T_tuple _ -> true | _ -> false)
     | None -> false)

let test_alias_is_transparent_for_shape _ =
  assert_exhaustive ~env:color
    [ ctor "Red" []; ctor "Green" []; alias (ctor "Blue" []) "seen" ];
  assert_not_exhaustive ~env:color
    [ alias (ctor "Red" []) "seen"; ctor "Green" [] ];
  assert_exhaustive ~env:color [ alias anything "whole" ];
  assert_exhaustive [ alias (cons anything anything) "whole"; nil ];
  assert_not_exhaustive [ alias (cons anything anything) "whole" ]

let test_alias_over_a_wildcard_is_redundant_after_it _ =
  assert_equal ~printer:(fun l -> String.concat "," (List.map string_of_int l))
    [ 1 ]
    (E.redundant_clauses color [ alias anything "whole"; ctor "Red" [] ])

let test_alias_binds_the_whole_value _ =
  let tree =
    E.build (fun _ -> None) [ alias (cons (var "h") (var "t")) "whole" ]
  in
  let rec bindings (node : E.Decision_tree.t) =
    match node with
    | E.Decision_tree.Leaf { bindings; _ } -> List.map fst bindings
    | E.Decision_tree.Switch { branches; default; _ } ->
        List.concat_map (fun (_, sub) -> bindings sub) branches
        @ (match default with None -> [] | Some sub -> bindings sub)
    | E.Decision_tree.Fail -> []
  in
  let bound = List.sort compare (bindings tree) in
  assert_equal ~printer:(String.concat ",") [ "h"; "t"; "whole" ] bound

let suite =
  [
    "alias_is_transparent_for_shape" >:: test_alias_is_transparent_for_shape;
    "alias_over_a_wildcard_is_redundant_after_it"
    >:: test_alias_over_a_wildcard_is_redundant_after_it;
    "alias_binds_the_whole_value" >:: test_alias_binds_the_whole_value;
    "witness_list_nil" >:: test_witness_list_nil;
    "witness_list_cons" >:: test_witness_list_cons;
    "witness_exhaustive" >:: test_witness_exhaustive;
    "witness_nonexhaustive" >:: test_witness_nonexhaustive;
    "witness_nested" >:: test_witness_nested;
    "witness_missing_ctor_named" >:: test_witness_missing_ctor_named;
    "witness_int_gap" >:: test_witness_int_gap;
    "witness_string_gap" >:: test_witness_string_gap;
    "witness_char_gap" >:: test_witness_char_gap;
    "witness_tuple" >:: test_witness_tuple;
    "empty_match" >:: test_empty_match;
    "unit_exhaustive" >:: test_unit_exhaustive;
    "record_exhaustive" >:: test_record_exhaustive;
    "record_before_wildcard_redundant" >:: test_record_before_wildcard_redundant;
    "record_inside_ctor" >:: test_record_inside_ctor;
    "str_no_wildcard" >:: test_str_no_wildcard;
    "str_with_wildcard" >:: test_str_with_wildcard;
    "chr_no_wildcard" >:: test_chr_no_wildcard;
    "chr_with_wildcard" >:: test_chr_with_wildcard;
    "list_literal_missing" >:: test_list_literal_missing;
    "list_literal_with_nil_and_cons" >:: test_list_literal_with_nil_and_cons;
    "redundant_duplicate" >:: test_redundant_duplicate;
    "redundant_after_wildcard" >:: test_redundant_after_wildcard;
    "no_redundant" >:: test_no_redundant;
    "enum_complete" >:: test_enum_complete;
    "enum_missing" >:: test_enum_missing;
    "enum_wildcard" >:: test_enum_wildcard;
    "enum_redundant_still_exhaustive" >:: test_enum_redundant_still_exhaustive;
    "wildcard_first_is_exhaustive" >:: test_wildcard_first_is_exhaustive;
    "bool_unregistered_is_incomplete" >:: test_bool_unregistered_is_incomplete;
    "bool_as_variant_is_complete" >:: test_bool_as_variant_is_complete;
    "nested_complete" >:: test_nested_complete;
    "nested_missing" >:: test_nested_missing;
    "nested_inner_wildcard" >:: test_nested_inner_wildcard;
    "payload_wildcards" >:: test_payload_wildcards;
    "list_complete" >:: test_list_complete;
    "list_only_cons" >:: test_list_only_cons;
    "list_only_nil" >:: test_list_only_nil;
    "list_nil_wildcard" >:: test_list_nil_wildcard;
    "list_nested_cons" >:: test_list_nested_cons;
    "int_no_wildcard" >:: test_int_no_wildcard;
    "int_with_wildcard" >:: test_int_with_wildcard;
    "single_var" >:: test_single_var;
    "tuple_complete" >:: test_tuple_complete;
    "tuple_missing" >:: test_tuple_missing;
    "tuple_component_wildcard" >:: test_tuple_component_wildcard;
  ]
