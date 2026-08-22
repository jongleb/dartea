open OUnit2

let rec ensured directory =
  if not (Sys.file_exists directory) then begin
    ensured (Filename.dirname directory);
    Sys.mkdir directory 0o755
  end

let written ~folder ~path content =
  let file = Filename.concat folder path in
  ensured (Filename.dirname file);
  Out_channel.with_open_bin file (fun out ->
      Out_channel.output_string out content)

let rendered sources (error : Reporting.Error.t) =
  Reporting.Report.to_string ~colours:false
    (Reporting.Sources.report sources error)

let loaded folder =
  Eio_main.run @@ fun env ->
  File_loader.Files.load Eio.Path.(Eio.Stdenv.fs env / folder)

let sources_of folder =
  match loaded folder with
  | Ok sources -> sources
  | Error error -> assert_failure (rendered Reporting.Sources.empty error)

let unexpected problem = assert_failure (Reporting.Error.show_problem problem)

let refused folder =
  match loaded folder with
  | Ok _ -> assert_failure "the folder loaded without an error"
  | Error { problem = Project problem; _ } -> problem
  | Error error -> unexpected error.problem

let names_printer names = String.concat ", " names
let sorted names = List.sort String.compare names

let names sources =
  sorted
    (List.map
       (fun (source : File_loader.Files.Elm_file.t) -> source.name)
       sources)

let paths sources =
  sorted
    (List.map
       (fun (source : File_loader.Files.Elm_file.t) -> source.path)
       sources)

let thing = {|module Deep.Thing exposing (answer)


answer : Int
answer =
    42
|}

let main = {|module Main exposing (main)

import Deep.Thing


main : Int
main =
    Deep.Thing.answer + 1
|}

let compiled sources =
  let outcome = Dartea.Compiler.compile_modules sources in
  match outcome.errors with
  | [] ->
      sorted
        (List.map
           (fun (module_ : Dartea.Compiler.compiled) -> module_.module_name)
           outcome.output)
  | error :: _ ->
      assert_failure (rendered (Reporting.Sources.of_list outcome.sources) error)

let application directories =
  Printf.sprintf {|{ "type": "application", "source-directories": [%s] }|}
    (String.concat ", " (List.map (Printf.sprintf {|"%s"|}) directories))

let project_with elm_json =
  let folder = Filename.temp_dir "dartea" "" in
  written ~folder ~path:"elm.json" elm_json;
  written ~folder ~path:"src/Main.elm" main;
  folder

let test_source_folder _ =
  let folder = Filename.temp_dir "dartea" "" in
  written ~folder ~path:"src/Main.elm" main;
  written ~folder ~path:"src/Deep/Thing.elm" thing;
  let sources = sources_of folder in
  assert_equal ~printer:names_printer [ "Deep.Thing"; "Main" ] (names sources);
  assert_equal ~printer:names_printer
    [ "src/Deep/Thing.elm"; "src/Main.elm" ]
    (paths sources);
  assert_bool "the project did not compile"
    (List.mem "Deep.Thing" (compiled sources))

let test_folder_without_src _ =
  let folder = Filename.temp_dir "dartea" "" in
  written ~folder ~path:"Main.elm" main;
  written ~folder ~path:"Deep/Thing.elm" thing;
  let sources = sources_of folder in
  assert_equal ~printer:names_printer [ "Deep.Thing"; "Main" ] (names sources);
  assert_equal ~printer:names_printer [ "Deep/Thing.elm"; "Main.elm" ] (paths sources);
  assert_bool "the project did not compile"
    (List.mem "Deep.Thing" (compiled sources))

let test_hidden_folders_are_skipped _ =
  let folder = Filename.temp_dir "dartea" "" in
  written ~folder ~path:"Main.elm" "main : Int\nmain =\n    1\n";
  written ~folder ~path:".hidden/Other.elm" "other : Int\nother =\n    2\n";
  assert_equal ~printer:names_printer [ "Main" ] (names (sources_of folder))

let test_name_must_match_the_path _ =
  let folder = Filename.temp_dir "dartea" "" in
  written ~folder ~path:"src/Deep/Other.elm" thing;
  let mismatched (error : Reporting.Error.t) =
    match error.problem with
    | Syntax (Module_name_mismatch { expected }) ->
        assert_equal ~printer:Fun.id "Deep.Other" expected
    | problem -> unexpected problem
  in
  match Dartea.Compiler.compile_modules (sources_of folder) with
  | outcome -> (
      match outcome.errors with
      | [] -> assert_failure "the mismatch compiled without an error"
      | error :: _ -> mismatched error)
  | exception Reporting.Error.Found error -> mismatched error

let test_empty_folder_is_refused _ =
  let folder = Filename.temp_dir "dartea" "" in
  match refused folder with
  | No_sources _ -> ()
  | problem -> unexpected (Project problem)

let test_source_directories _ =
  let folder = Filename.temp_dir "dartea" "" in
  written ~folder ~path:"elm.json" (application [ "app"; "vendor" ]);
  written ~folder ~path:"app/Main.elm" main;
  written ~folder ~path:"vendor/Deep/Thing.elm" thing;
  let sources = sources_of folder in
  assert_equal ~printer:names_printer [ "Deep.Thing"; "Main" ] (names sources);
  assert_equal ~printer:names_printer
    [ "app/Main.elm"; "vendor/Deep/Thing.elm" ]
    (paths sources);
  assert_bool "the project did not compile"
    (List.mem "Deep.Thing" (compiled sources))

let test_package_looks_in_src _ =
  let folder = Filename.temp_dir "dartea" "" in
  written ~folder ~path:"elm.json" {|{ "type": "package", "name": "gleb/thing" }|};
  written ~folder ~path:"src/Deep/Thing.elm" thing;
  assert_equal ~printer:names_printer [ "Deep.Thing" ] (names (sources_of folder))

let test_broken_json _ =
  match refused (project_with {|{ "type": "application", }|}) with
  | Bad_json _ -> ()
  | problem -> unexpected (Project problem)

let test_missing_type _ =
  match refused (project_with {|{ "source-directories": ["src"] }|}) with
  | Missing_field { field; _ } -> assert_equal ~printer:Fun.id "type" field
  | problem -> unexpected (Project problem)

let test_source_directories_must_be_strings _ =
  match
    refused
      (project_with {|{ "type": "application", "source-directories": [1] }|})
  with
  | Bad_field { field; _ } ->
      assert_equal ~printer:Fun.id "source-directories" field
  | problem -> unexpected (Project problem)

let test_source_directory_must_exist _ =
  match refused (project_with (application [ "src"; "vendor" ])) with
  | Missing_source_directory { folder; _ } ->
      assert_equal ~printer:Fun.id "vendor" folder
  | problem -> unexpected (Project problem)

let test_one_module_per_name _ =
  let folder = Filename.temp_dir "dartea" "" in
  written ~folder ~path:"elm.json" (application [ "app"; "vendor" ]);
  written ~folder ~path:"app/Deep/Thing.elm" thing;
  written ~folder ~path:"vendor/Deep/Thing.elm" thing;
  match refused folder with
  | Duplicate_module { name; one; other } ->
      assert_equal ~printer:Fun.id "Deep.Thing" name;
      assert_equal ~printer:names_printer
        [ "app/Deep/Thing.elm"; "vendor/Deep/Thing.elm" ]
        [ one; other ]
  | problem -> unexpected (Project problem)

let driver = Filename.concat ".." (Filename.concat "bin" "main.exe")

let exit_code = function
  | Unix.WEXITED code -> code
  | Unix.WSIGNALED code | Unix.WSTOPPED code -> -code

let test_empty_folder_stops_the_driver _ =
  let folder = Filename.temp_dir "dartea" "" in
  let command =
    Printf.sprintf "NO_COLOR=1 %s %s 2>&1" (Filename.quote driver)
      (Filename.quote folder)
  in
  let channel = Unix.open_process_in command in
  let printed = Node_runner.read_all channel in
  let status = Unix.close_process_in channel in
  assert_equal ~printer:string_of_int 1 (exit_code status);
  assert_bool printed (Node_runner.contains ~needle:"NO SOURCE FILES" printed)

let suite =
  [
    "source_folder" >:: test_source_folder;
    "folder_without_src" >:: test_folder_without_src;
    "hidden_folders_are_skipped" >:: test_hidden_folders_are_skipped;
    "name_must_match_the_path" >:: test_name_must_match_the_path;
    "empty_folder_is_refused" >:: test_empty_folder_is_refused;
    "source_directories" >:: test_source_directories;
    "package_looks_in_src" >:: test_package_looks_in_src;
    "broken_json" >:: test_broken_json;
    "missing_type" >:: test_missing_type;
    "source_directories_must_be_strings" >:: test_source_directories_must_be_strings;
    "source_directory_must_exist" >:: test_source_directory_must_exist;
    "one_module_per_name" >:: test_one_module_per_name;
    "empty_folder_stops_the_driver" >:: test_empty_folder_stops_the_driver;
  ]
