open OUnit2
module Blocks = Codegen_js.Blocks

let main_of folder =
  let outcome = Sample.compiled_in ~entry:None (Filename.concat Sample.playground_root folder) in
  match outcome.errors with
  | [] ->
      List.find
        (fun (module_ : Dartea.Compiler.linkable) ->
          String.equal module_.module_name "Main")
        outcome.modules
  | errors -> Sample.refused (Reporting.Sources.of_list outcome.sources) errors

let declaration ~name folder =
  let main = main_of folder in
  List.find
    (fun (d : Optimized.Declaration.t) ->
      String.equal (Data.Located.unwrap d.name) name)
    main.declarations

let forms_in ~name folder = Blocks.forms (declaration ~name folder).body

let count kind holes =
  List.length
    (List.filter (fun (hole : Blocks.hole) -> Blocks.equal_hole_kind hole.kind kind) holes)

let show (forms : (Blocks.t * Blocks.hole list) list) =
  let holes = List.concat_map snd forms in
  Printf.sprintf "forms %d, nodes %d, text %d, attribute %d, event %d, children %d, subtree %d"
    (List.length forms)
    (List.fold_left (fun total (form, _) -> total + Blocks.size form) 0 forms)
    (count Text holes) (count Attribute holes) (count Event holes)
    (count Children holes) (count Subtree holes)

let assert_shape ~expected forms =
  assert_equal ~printer:Fun.id expected (show forms)

let test_counter_view _ =
  assert_shape
    ~expected:"forms 1, nodes 3, text 1, attribute 0, event 2, children 0, subtree 0"
    (forms_in ~name:"view" "counter")

let test_todomvc_view _ =
  assert_shape
    ~expected:"forms 1, nodes 2, text 0, attribute 0, event 0, children 0, subtree 3"
    (forms_in ~name:"view" "todomvc")

let test_todomvc_view_input _ =
  assert_shape
    ~expected:"forms 1, nodes 3, text 0, attribute 4, event 0, children 0, subtree 0"
    (forms_in ~name:"viewInput" "todomvc")

let test_todomvc_view_entries _ =
  assert_shape
    ~expected:"forms 1, nodes 3, text 0, attribute 2, event 1, children 0, subtree 1"
    (forms_in ~name:"viewEntries" "todomvc")

let test_todomvc_view_entry _ =
  assert_shape
    ~expected:"forms 1, nodes 6, text 1, attribute 6, event 4, children 0, subtree 0"
    (forms_in ~name:"viewEntry" "todomvc")

let test_todomvc_view_controls _ =
  assert_shape
    ~expected:"forms 1, nodes 1, text 0, attribute 1, event 0, children 0, subtree 3"
    (forms_in ~name:"viewControls" "todomvc")

let test_bench_view_row _ =
  assert_shape
    ~expected:"forms 1, nodes 7, text 2, attribute 1, event 2, children 0, subtree 0"
    (forms_in ~name:"viewRow" "bench")

let test_bench_view _ =
  assert_shape
    ~expected:"forms 1, nodes 10, text 0, attribute 0, event 6, children 0, subtree 1"
    (forms_in ~name:"view" "bench")

let compile source =
  let outcome = Dartea.Compiler.compile_source source in
  match outcome.errors with
  | [] ->
      List.find
        (fun (module_ : Dartea.Compiler.linkable) ->
          String.equal module_.module_name "Main")
        outcome.modules
  | errors -> Sample.refused (Reporting.Sources.of_list outcome.sources) errors

let body_of ~name source =
  let main = compile source in
  (List.find
     (fun (d : Optimized.Declaration.t) ->
       String.equal (Data.Located.unwrap d.name) name)
     main.declarations)
    .body

let test_refuse_dynamic_tag _ =
  let source =
    {|import Html exposing (Html)

view : String -> Html msg
view tag =
    Html.node tag [] []
|}
  in
  assert_bool "a dynamic tag is not a form"
    (Option.is_none (Blocks.of_expression (body_of ~name:"view" source)))

let test_refuse_dynamic_attributes _ =
  let source =
    {|import Html exposing (Html, Attribute, div)

attrs : Int -> List (Attribute msg)
attrs model =
    if model > 0 then
        []

    else
        attrs (model + 1)

view : Int -> Html msg
view model =
    div (attrs model) []
|}
  in
  assert_bool "a dynamic attribute list is not a form"
    (Option.is_none (Blocks.of_expression (body_of ~name:"view" source)))

let suite =
  [
    "counter_view" >:: test_counter_view;
    "todomvc_view" >:: test_todomvc_view;
    "todomvc_view_input" >:: test_todomvc_view_input;
    "todomvc_view_entries" >:: test_todomvc_view_entries;
    "todomvc_view_entry" >:: test_todomvc_view_entry;
    "todomvc_view_controls" >:: test_todomvc_view_controls;
    "bench_view_row" >:: test_bench_view_row;
    "bench_view" >:: test_bench_view;
    "refuse_dynamic_tag" >:: test_refuse_dynamic_tag;
    "refuse_dynamic_attributes" >:: test_refuse_dynamic_attributes;
  ]
