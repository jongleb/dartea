open OUnit2

let source content = Project.Elm_file.of_path ~path:"Main.elm" content

let outcome_of ~entry content =
  let outcome = Dartea.Compiler.compile_modules ~entry [ source content ] in
  match outcome.errors with
  | [] -> outcome
  | error :: _ ->
      assert_failure
        (Reporting.Report.to_string ~colours:false
           (Reporting.Sources.report
              (Reporting.Sources.of_list outcome.sources)
              error))

let delivered ~delivery (outcome : Dartea.Compiler.outcome) =
  let module Delivery = (val delivery : Dartea.Delivery.S) in
  Delivery.files ~entry:outcome.entry outcome.output

let refused ~delivery outcome =
  match delivered ~delivery outcome with
  | _ -> assert_failure "the delivery accepted the entry point"
  | exception Reporting.Error.Found { problem = Project problem; _ } -> problem
  | exception Reporting.Error.Found error ->
      assert_failure (Reporting.Error.show_problem error.problem)

let paths files =
  List.map (fun (file : Dartea.Delivery.file) -> file.path) files
  |> List.sort String.compare

let browser = Dartea.Delivery.find "classic_js_browser"

let test_esm_folder_is_one_file_per_module _ =
  let outcome = outcome_of ~entry:None Sample.starter in
  let files = delivered ~delivery:Dartea.Delivery.default outcome in
  assert_equal ~printer:Sample.names
    (Dartea.Delivery.licence_file
     :: List.map
          (fun (module_ : Dartea.Compiler.compiled) ->
            Codegen_js.Of_optimized.module_file module_.module_name)
          outcome.output
    |> List.sort String.compare)
    (paths files);
  List.iter2
    (fun (module_ : Dartea.Compiler.compiled) (file : Dartea.Delivery.file) ->
      assert_equal ~printer:Fun.id ~msg:module_.module_name module_.source
        file.content)
    outcome.output
    (List.filter
       (fun (file : Dartea.Delivery.file) ->
         not (String.equal file.path Dartea.Delivery.licence_file))
       files)

let test_browser_writes_a_page_and_a_bundle _ =
  let outcome = outcome_of ~entry:(Some "Main") Sample.program in
  assert_equal ~printer:Sample.names
    [ "build/dartea.LICENSE.txt"; "build/index.html"; "build/main.js" ]
    (paths (delivered ~delivery:browser outcome))

let written directory (file : Dartea.Delivery.file) =
  Sample.written ~folder:directory ~path:file.path file.content

let printed_by ~program ~stub =
  let outcome = outcome_of ~entry:(Some "Main") program in
  let directory = Sample.folder () in
  List.iter (written directory) (delivered ~delivery:browser outcome);
  Sample.written
    ~folder:(Filename.concat directory "build")
    ~path:"stub.mjs" stub;
  let command =
    Printf.sprintf "cd %s && node stub.mjs 2>&1"
      (Filename.quote (Filename.concat directory "build"))
  in
  let channel = Unix.open_process_in command in
  let printed = Node_runner.read_all channel in
  let (_ : Unix.process_status) = Unix.close_process_in channel in
  String.trim printed

let test_typing_reaches_the_model _ =
  assert_equal ~printer:Fun.id
    {|<ok> -> <ok!> (stopped 1, prevented 1, value "ok!")|}
    (printed_by ~program:Sample.typing_program ~stub:Sample.typing_stub)

let test_the_counter_reacts_to_a_click _ =
  assert_equal ~printer:Fun.id
    "+0 -> +2 (created 0, replaced 0, same node true)"
    (printed_by ~program:Sample.program ~stub:Sample.dom_stub)

let test_mapped_messages_reach_the_model _ =
  assert_equal ~printer:Fun.id
    "pokepokeattr[L0RAL4] (created 0 , same buttons true )"
    (printed_by ~program:Sample.mapped_program ~stub:Sample.mapped_stub)

let test_a_mapped_subtree_survives_being_replaced _ =
  assert_equal ~printer:Fun.id "flipblock[PP]"
    (printed_by ~program:Sample.reshaped_program ~stub:Sample.reshaped_stub)

let test_keyed_list_moves_nodes_instead_of_making_them _ =
  assert_equal ~printer:Fun.id
    "-10,1,2,3,4,5,6,7,8,9,10 (created 1, kept true) -> \
     10,9,8,7,6,5,4,3,2,1,-10 (created 0, kept true)"
    (printed_by ~program:Sample.keyed_program ~stub:Sample.keyed_stub)

let test_value_lands_on_the_property _ =
  assert_equal ~printer:Fun.id
    {|property "3", attribute undefined, class "wrap"|}
    (printed_by ~program:Sample.field_program ~stub:Sample.property_stub)

let test_browser_needs_an_entry _ =
  match refused ~delivery:browser (outcome_of ~entry:None Sample.program) with
  | Delivery_needs_entry { delivery } ->
      assert_equal ~printer:Fun.id "classic_js_browser" delivery
  | problem -> assert_failure (Reporting.Project_error.show problem)

let test_browser_needs_an_exposed_entry _ =
  let hidden = {|module Main exposing (other)


other : Int
other =
    1


main : String
main =
    "hi"
|} in
  match refused ~delivery:browser (outcome_of ~entry:(Some "Main") hidden) with
  | Entry_not_exposed { delivery; module_name; declaration } ->
      assert_equal ~printer:Fun.id "classic_js_browser" delivery;
      assert_equal ~printer:Fun.id "Main" module_name;
      assert_equal ~printer:Fun.id "main" declaration
  | problem -> assert_failure (Reporting.Project_error.show problem)

let test_browser_needs_a_program _ =
  match
    refused ~delivery:browser (outcome_of ~entry:(Some "Main") Sample.starter)
  with
  | Bad_entry { expected; found; _ } ->
      assert_equal ~printer:Fun.id "Browser.Program" expected;
      assert_equal ~printer:Fun.id "String" found
  | problem -> assert_failure (Reporting.Project_error.show problem)

let test_unknown_delivery _ =
  match Dartea.Delivery.find "nope" with
  | _ -> assert_failure "an unknown delivery was accepted"
  | exception
      Reporting.Error.Found
        { problem = Project (Unknown_delivery { name; known }); _ } ->
      assert_equal ~printer:Fun.id "nope" name;
      assert_equal ~printer:Sample.names
        [ "classic_js_browser"; "esm_folder" ]
        (List.sort String.compare known)
  | exception Reporting.Error.Found error ->
      assert_failure (Reporting.Error.show_problem error.problem)

let suite =
  [
    "esm_folder_is_one_file_per_module"
    >:: test_esm_folder_is_one_file_per_module;
    "browser_writes_a_page_and_a_bundle"
    >:: test_browser_writes_a_page_and_a_bundle;
    "the_counter_reacts_to_a_click" >:: test_the_counter_reacts_to_a_click;
    "typing_reaches_the_model" >:: test_typing_reaches_the_model;
    "mapped_messages_reach_the_model" >:: test_mapped_messages_reach_the_model;
    "a_mapped_subtree_survives_being_replaced"
    >:: test_a_mapped_subtree_survives_being_replaced;
    "keyed_list_moves_nodes_instead_of_making_them"
    >:: test_keyed_list_moves_nodes_instead_of_making_them;
    "value_lands_on_the_property" >:: test_value_lands_on_the_property;
    "browser_needs_an_entry" >:: test_browser_needs_an_entry;
    "browser_needs_an_exposed_entry" >:: test_browser_needs_an_exposed_entry;
    "browser_needs_a_program" >:: test_browser_needs_a_program;
    "unknown_delivery" >:: test_unknown_delivery;
  ]
