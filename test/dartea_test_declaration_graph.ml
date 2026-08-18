open OUnit2

let canonical input =
  match Parse.Main.parse ~file:"Main.elm" input with
  | Error error -> raise (Reporting.Error.Found error)
  | Ok impl_list ->
      Canonical.Module.of_frontend ~fallback_name:"Main"
        (Ast.Kind.Frontend.Module.of_impl impl_list)

let declarations source =
  (canonical source).Canonical.Module.top_declarations

let named grouped =
  List.map
    (List.map (fun (d : Canonical.Declaration.t) ->
         Data.Located.unwrap d.body_part.name))
    grouped

let groups source =
  named
    (Canonicalization.Declaration_graph.in_dependency_order ~declaration:Fun.id
       (declarations source))

let assert_groups ~src ~expected =
  assert_equal
    ~printer:(fun grouped ->
      String.concat " | "
        (List.map (fun group -> String.concat ", " group) grouped))
    expected (groups src)

let test_a_chain_is_ordered_by_dependency _ =
  assert_groups
    ~src:{|
first = second

second = third

third = 1
|}
    ~expected:[ [ "third" ]; [ "second" ]; [ "first" ] ]

let test_mutual_recursion_is_one_group _ =
  assert_groups
    ~src:
      {|
isEven n = if n == 0 then True else isOdd (n - 1)

isOdd n = if n == 0 then False else isEven (n - 1)
|}
    ~expected:[ [ "isEven"; "isOdd" ] ]

let test_self_recursion_is_a_group_of_one _ =
  assert_groups
    ~src:{|
countdown n = if n == 0 then 0 else countdown (n - 1)
|}
    ~expected:[ [ "countdown" ] ]

let test_independent_declarations_keep_the_order_they_arrive_in _ =
  let given = declarations {|
zebra = 1

alpha = 2

middle = 3
|} in
  let ordered given =
    named
      (Canonicalization.Declaration_graph.in_dependency_order ~declaration:Fun.id
         given)
  in
  assert_equal
    ~printer:(String.concat ", ")
    [ "zebra"; "alpha"; "middle" ]
    (List.concat (ordered given));
  assert_equal
    ~printer:(String.concat ", ")
    [ "middle"; "alpha"; "zebra" ]
    (List.concat (ordered (List.rev given)))

let test_a_dependency_comes_before_its_user_whatever_the_name _ =
  assert_groups
    ~src:{|
divides d n = rem n d

rem a b = a - b
|}
    ~expected:[ [ "rem" ]; [ "divides" ] ]

let test_a_group_comes_after_what_it_depends_on _ =
  assert_groups
    ~src:
      {|
shared = 1

ping n = pong (n - shared)

pong n = ping (n - shared)

using = ping 3
|}
    ~expected:[ [ "shared" ]; [ "ping"; "pong" ]; [ "using" ] ]

let test_imported_names_are_not_edges _ =
  assert_groups
    ~src:{|
size text = String.length text
|}
    ~expected:[ [ "size" ] ]

let test_a_constructor_in_a_pattern_is_not_an_edge _ =
  assert_groups
    ~src:
      {|
describing subject =
    case subject of
        Nothing -> 0
        Just payload -> payload
|}
    ~expected:[ [ "describing" ] ]

let test_a_shadowed_name_is_not_an_edge _ =
  assert_groups
    ~src:{|
later = 1

shadowing later = later
|}
    ~expected:[ [ "later" ]; [ "shadowing" ] ]

let suite =
  [
    "a_chain_is_ordered_by_dependency" >:: test_a_chain_is_ordered_by_dependency;
    "mutual_recursion_is_one_group" >:: test_mutual_recursion_is_one_group;
    "self_recursion_is_a_group_of_one" >:: test_self_recursion_is_a_group_of_one;
    "independent_declarations_keep_the_order_they_arrive_in"
    >:: test_independent_declarations_keep_the_order_they_arrive_in;
    "a_dependency_comes_before_its_user_whatever_the_name"
    >:: test_a_dependency_comes_before_its_user_whatever_the_name;
    "a_group_comes_after_what_it_depends_on"
    >:: test_a_group_comes_after_what_it_depends_on;
    "imported_names_are_not_edges" >:: test_imported_names_are_not_edges;
    "a_constructor_in_a_pattern_is_not_an_edge"
    >:: test_a_constructor_in_a_pattern_is_not_an_edge;
    "a_shadowed_name_is_not_an_edge" >:: test_a_shadowed_name_is_not_an_edge;
  ]
