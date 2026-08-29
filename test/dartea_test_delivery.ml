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

let delivered ~delivery ~output outcome =
  Dartea.Delivery.produced ~delivery ~output outcome

let refused ~delivery ~output outcome =
  match delivered ~delivery ~output outcome with
  | _ -> assert_failure "the delivery accepted the entry point"
  | exception Reporting.Error.Found { problem = Project problem; _ } -> problem
  | exception Reporting.Error.Found error ->
      assert_failure (Reporting.Error.show_problem error.problem)

let paths files =
  List.map (fun (file : Dartea.Delivery.file) -> file.path) files
  |> List.sort String.compare

let page : (module Dartea.Delivery.S) = (module Dartea.Delivery.Sandwich)
let script : (module Dartea.Delivery.S) = (module Dartea.Delivery.Script)

let test_esm_folder_is_one_file_per_module _ =
  let outcome = outcome_of ~entry:None Sample.starter in
  let files = delivered ~delivery:Dartea.Delivery.default ~output:"." outcome in
  assert_equal ~printer:Sample.names
    (("./" ^ Dartea.Delivery.licence_file)
     :: List.map
          (fun (module_name, _) ->
            "./" ^ Codegen_js.Of_optimized.module_file module_name)
          (Sample.delivered ~delivery:Dartea.Delivery.default outcome)
    |> List.sort String.compare)
    (paths files);
  List.iter2
    (fun (module_name, source) (file : Dartea.Delivery.file) ->
      assert_equal ~printer:Fun.id ~msg:module_name source file.content)
    (Sample.delivered ~delivery:Dartea.Delivery.default outcome)
    (List.filter
       (fun (file : Dartea.Delivery.file) ->
         not
           (String.equal file.path ("./" ^ Dartea.Delivery.licence_file)))
       files)

let test_a_page_is_one_file _ =
  let outcome = outcome_of ~entry:(Some "Main") Sample.program in
  assert_equal ~printer:Sample.names
    [ "build/dartea.LICENSE.txt"; "build/index.html" ]
    (paths (delivered ~delivery:page ~output:"build/index.html" outcome));
  assert_equal ~printer:Sample.names
    [ "build/dartea.LICENSE.txt"; "build/main.js" ]
    (paths (delivered ~delivery:script ~output:"build/main.js" outcome))

let written directory (file : Dartea.Delivery.file) =
  Sample.written ~folder:directory ~path:file.path file.content

let printed ~outcome ~stub =
  let directory = Sample.folder () in
  List.iter (written directory)
    (delivered ~delivery:script ~output:"build/main.js" outcome);
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

let printed_by ~program ~stub =
  printed ~outcome:(outcome_of ~entry:(Some "Main") program) ~stub

let printed_from ~folder ~stub =
  printed
    ~outcome:
      (Sample.compiled_in ~entry:(Some "Main")
         (Filename.concat Sample.playground_root folder))
    ~stub

let test_the_real_todomvc_runs _ =
  assert_equal ~printer:Fun.id
    "Elm • TodoMVC | added milk / cat | edited kefir / cat | left cat / All \
     | 1 item left"
    (printed_from ~folder:"todomvc" ~stub:Sample.todomvc_stub)

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

let test_a_point_change_writes_once _ =
  assert_equal ~printer:Fun.id
    {|bump 0/0/0/1 | paint 0/1/0/0 | strip 0/0/1/0 | "bps1"|}
    (printed_by ~program:Sample.counted_program ~stub:Sample.counted_stub)

let test_tasks_run_in_order _ =
  assert_equal ~printer:Fun.id "ok:ab!recovered err:boom ok:123"
    (printed_by ~program:Sample.task_program ~stub:Sample.task_stub)

let test_a_mapped_command_keeps_its_tagger _ =
  assert_equal ~printer:Fun.id "ok:one err:two"
    (printed_by ~program:Sample.mapped_command_program ~stub:Sample.task_stub)

let test_a_subscription_outlives_updates _ =
  assert_equal ~printer:Fun.id
    "timers 1 | ticks 2 | still 1 | after stop 0 | silent 2"
    (printed_by ~program:Sample.ticking_program ~stub:Sample.ticking_stub)

let test_ports_carry_both_ways _ =
  assert_equal ~printer:Fun.id
    {|sendfromJsmore | outgoing ["out:fromJs"]|}
    (printed_by ~program:Sample.port_program ~stub:Sample.port_stub)

let test_guards_stay_silent_like_elm _ =
  assert_equal ~printer:Fun.id
    {|script p | svg http://www.w3.org/2000/svg | xlink http://www.w3.org/1999/xlink #icon | viewBox undefined | hrefs ["","","","","/safe"] | onclick {"data-onclick":"boom()"} | innerHTML "<b>"|}
    (printed_by ~program:Sample.guarded_program ~stub:Sample.guarded_stub)

let test_lazy_skips_an_unchanged_subtree _ =
  assert_equal ~printer:Fun.id
    "br1-!-!2-!23-!234-!2345-!23456-!234567-!2345678 (skipped 0, recomputed 8)"
    (printed_by ~program:Sample.lazy_program ~stub:Sample.lazy_stub)

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
  match
    refused ~delivery:page ~output:"index.html"
      (outcome_of ~entry:None Sample.program)
  with
  | Delivery_needs_entry { delivery } ->
      assert_equal ~printer:Fun.id "page" delivery
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
  match refused ~delivery:page ~output:"index.html"
      (outcome_of ~entry:(Some "Main") hidden) with
  | Entry_not_exposed { delivery; module_name; declaration } ->
      assert_equal ~printer:Fun.id "page" delivery;
      assert_equal ~printer:Fun.id "Main" module_name;
      assert_equal ~printer:Fun.id "main" declaration
  | problem -> assert_failure (Reporting.Project_error.show problem)

let test_browser_needs_a_program _ =
  match
    refused ~delivery:page ~output:"index.html"
      (outcome_of ~entry:(Some "Main") Sample.starter)
  with
  | Bad_entry { expected; found; _ } ->
      assert_equal ~printer:Fun.id "Platform.Program" expected;
      assert_equal ~printer:Fun.id "String" found
  | problem -> assert_failure (Reporting.Project_error.show problem)

let suite =
  [
    "esm_folder_is_one_file_per_module"
    >:: test_esm_folder_is_one_file_per_module;
    "a_page_is_one_file" >:: test_a_page_is_one_file;
    "the_counter_reacts_to_a_click" >:: test_the_counter_reacts_to_a_click;
    "typing_reaches_the_model" >:: test_typing_reaches_the_model;
    "mapped_messages_reach_the_model" >:: test_mapped_messages_reach_the_model;
    "a_point_change_writes_once" >:: test_a_point_change_writes_once;
    "tasks_run_in_order" >:: test_tasks_run_in_order;
    "ports_carry_both_ways" >:: test_ports_carry_both_ways;
    "a_subscription_outlives_updates" >:: test_a_subscription_outlives_updates;
    "a_mapped_command_keeps_its_tagger"
    >:: test_a_mapped_command_keeps_its_tagger;
    "guards_stay_silent_like_elm" >:: test_guards_stay_silent_like_elm;
    "the_real_todomvc_runs" >:: test_the_real_todomvc_runs;
    "the_real_todomvc_runs" >:: test_the_real_todomvc_runs;
    "lazy_skips_an_unchanged_subtree" >:: test_lazy_skips_an_unchanged_subtree;
    "a_mapped_subtree_survives_being_replaced"
    >:: test_a_mapped_subtree_survives_being_replaced;
    "keyed_list_moves_nodes_instead_of_making_them"
    >:: test_keyed_list_moves_nodes_instead_of_making_them;
    "value_lands_on_the_property" >:: test_value_lands_on_the_property;
    "browser_needs_an_entry" >:: test_browser_needs_an_entry;
    "browser_needs_an_exposed_entry" >:: test_browser_needs_an_exposed_entry;
    "browser_needs_a_program" >:: test_browser_needs_a_program;
  ]
