open OUnit2

let loaded folder =
  Project.Sources.load ~provided:Prelude.packages (Fpath.v folder)

let sources_of folder =
  match loaded folder with
  | Ok sources -> sources
  | Error error ->
      assert_failure
        (Sample.rendered Reporting.Sources.empty
           (Reporting.Error.of_failure error))

let unexpected problem = assert_failure (Reporting.Error.show_problem problem)

let refused folder =
  match loaded folder with
  | Ok _ -> assert_failure "the folder loaded without an error"
  | Error failure -> failure.problem

let names sources =
  Sample.sorted
    (List.map
       (fun (source : Project.Elm_file.t) -> source.name)
       (Project.Sources.files sources))

let paths sources =
  Sample.sorted
    (List.map
       (fun (source : Project.Elm_file.t) -> source.path)
       (Project.Sources.files sources))

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
  let outcome = Dartea.Compiler.compile_modules ~entry:None sources in
  match outcome.errors with
  | [] ->
      Sample.sorted
        (List.map
           (fun (module_ : Dartea.Compiler.artifact) -> module_.module_name)
           (Node_runner.output_of outcome))
  | error :: _ ->
      assert_failure
        (Sample.rendered (Reporting.Sources.of_list outcome.sources) error)

let application directories =
  Printf.sprintf {|{ "type": "application", "source-directories": [%s] }|}
    (String.concat ", " (List.map (Printf.sprintf {|"%s"|}) directories))

let depending_on packages =
  let entry (name, version) = Printf.sprintf {|"%s": "%s"|} name version in
  Printf.sprintf
    {|{ "type": "application"
      , "source-directories": ["src"]
      , "dependencies":
          { "direct": {%s}, "indirect": {} }
      }|}
    (String.concat ", " (List.map entry packages))

let vendored ~folder ~package ~path contents =
  Sample.written ~folder
    ~path:(Printf.sprintf ".dartea/packages/%s/src/%s" package path)
    contents

let roots_of sources =
  let outcome = Dartea.Compiler.compile_modules ~entry:None sources in
  match outcome.errors with
  | [] -> Sample.sorted outcome.written
  | error :: _ ->
      assert_failure
        (Sample.rendered (Reporting.Sources.of_list outcome.sources) error)

let project_with elm_json =
  let folder = Sample.folder () in
  Sample.written ~folder ~path:"elm.json" elm_json;
  Sample.written ~folder ~path:"src/Main.elm" main;
  folder

let test_source_folder _ =
  let folder = Sample.folder () in
  Sample.written ~folder ~path:"src/Main.elm" main;
  Sample.written ~folder ~path:"src/Deep/Thing.elm" thing;
  let sources = sources_of folder in
  assert_equal ~printer:Sample.names [ "Deep.Thing"; "Main" ] (names sources);
  assert_equal ~printer:Sample.names
    [ "src/Deep/Thing.elm"; "src/Main.elm" ]
    (paths sources);
  assert_bool "the project did not compile"
    (List.mem "Deep.Thing" (compiled sources))

let test_folder_without_src _ =
  let folder = Sample.folder () in
  Sample.written ~folder ~path:"Main.elm" main;
  Sample.written ~folder ~path:"Deep/Thing.elm" thing;
  let sources = sources_of folder in
  assert_equal ~printer:Sample.names [ "Deep.Thing"; "Main" ] (names sources);
  assert_equal ~printer:Sample.names
    [ "Deep/Thing.elm"; "Main.elm" ]
    (paths sources);
  assert_bool "the project did not compile"
    (List.mem "Deep.Thing" (compiled sources))

let test_hidden_folders_are_skipped _ =
  let folder = Sample.folder () in
  Sample.written ~folder ~path:"Main.elm" "main : Int\nmain =\n    1\n";
  Sample.written ~folder ~path:".hidden/Other.elm"
    "other : Int\nother =\n    2\n";
  assert_equal ~printer:Sample.names [ "Main" ] (names (sources_of folder))

let test_name_must_match_the_path _ =
  let folder = Sample.folder () in
  Sample.written ~folder ~path:"src/Deep/Other.elm" thing;
  let mismatched (error : Reporting.Error.t) =
    match error.problem with
    | Syntax (Module_name_mismatch { expected }) ->
        assert_equal ~printer:Fun.id "Deep.Other" expected
    | problem -> unexpected problem
  in
  match Dartea.Compiler.compile_modules ~entry:None (sources_of folder) with
  | outcome -> (
      match outcome.errors with
      | [] -> assert_failure "the mismatch compiled without an error"
      | error :: _ -> mismatched error)
  | exception Reporting.Error.Found error -> mismatched error

let test_empty_folder_is_refused _ =
  let folder = Sample.folder () in
  match refused folder with
  | No_sources _ -> ()
  | problem -> unexpected (Project problem)

let test_source_directories _ =
  let folder = Sample.folder () in
  Sample.written ~folder ~path:"elm.json" (application [ "app"; "vendor" ]);
  Sample.written ~folder ~path:"app/Main.elm" main;
  Sample.written ~folder ~path:"vendor/Deep/Thing.elm" thing;
  let sources = sources_of folder in
  assert_equal ~printer:Sample.names [ "Deep.Thing"; "Main" ] (names sources);
  assert_equal ~printer:Sample.names
    [ "app/Main.elm"; "vendor/Deep/Thing.elm" ]
    (paths sources);
  assert_bool "the project did not compile"
    (List.mem "Deep.Thing" (compiled sources))

let test_package_looks_in_src _ =
  let folder = Sample.folder () in
  Sample.written ~folder ~path:"elm.json"
    {|{ "type": "package", "name": "gleb/thing" }|};
  Sample.written ~folder ~path:"src/Deep/Thing.elm" thing;
  assert_equal ~printer:Sample.names [ "Deep.Thing" ]
    (names (sources_of folder))

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
  let folder = Sample.folder () in
  Sample.written ~folder ~path:"elm.json" (application [ "app"; "vendor" ]);
  Sample.written ~folder ~path:"app/Deep/Thing.elm" thing;
  Sample.written ~folder ~path:"vendor/Deep/Thing.elm" thing;
  match refused folder with
  | Duplicate_module { name; one; other } ->
      assert_equal ~printer:Fun.id "Deep.Thing" name;
      assert_equal ~printer:Sample.names
        [ "app/Deep/Thing.elm"; "vendor/Deep/Thing.elm" ]
        [ one; other ]
  | problem -> unexpected (Project problem)

let test_dependency_is_taken_from_disk _ =
  let folder = Sample.folder () in
  Sample.written ~folder ~path:"elm.json"
    (depending_on [ ("gleb/thing", "1.0.0") ]);
  Sample.written ~folder ~path:"src/Main.elm" main;
  vendored ~folder ~package:"gleb/thing/1.0.0" ~path:"Deep/Thing.elm" thing;
  let sources = sources_of folder in
  assert_equal ~printer:Sample.names [ "Deep.Thing"; "Main" ] (names sources);
  assert_equal ~printer:Sample.names
    [ ".dartea/packages/gleb/thing/1.0.0/src/Deep/Thing.elm"; "src/Main.elm" ]
    (paths sources);
  assert_bool "the project did not compile"
    (List.mem "Deep.Thing" (compiled sources))

let test_a_package_is_not_a_root _ =
  let folder = Sample.folder () in
  Sample.written ~folder ~path:"elm.json"
    (depending_on [ ("gleb/thing", "1.0.0") ]);
  Sample.written ~folder ~path:"src/Main.elm" main;
  vendored ~folder ~package:"gleb/thing/1.0.0" ~path:"Deep/Thing.elm" thing;
  assert_equal ~printer:Sample.names [ "Main" ] (roots_of (sources_of folder))

let test_our_own_packages_are_skipped _ =
  let folder = Sample.folder () in
  Sample.written ~folder ~path:"elm.json"
    (depending_on [ ("elm/core", "1.0.5"); ("elm/json", "1.1.3") ]);
  Sample.written ~folder ~path:"src/Main.elm" Sample.starter;
  assert_equal ~printer:Sample.names [ "Main" ] (names (sources_of folder))

let test_a_missing_package_is_refused _ =
  let folder = Sample.folder () in
  Sample.written ~folder ~path:"elm.json"
    (depending_on [ ("gleb/thing", "1.0.0") ]);
  Sample.written ~folder ~path:"src/Main.elm" Sample.starter;
  match refused folder with
  | Missing_package { package; version; looked; _ } ->
      assert_equal ~printer:Fun.id "gleb/thing" package;
      assert_equal ~printer:Fun.id "1.0.0" version;
      assert_equal ~printer:Fun.id ".dartea/packages/gleb/thing/1.0.0/src"
        looked
  | problem -> unexpected (Project problem)

let manifest packages =
  let entry (name, range) = Printf.sprintf {|"%s": "%s"|} name range in
  Printf.sprintf {|{ "name": "app", "dependencies": {%s} }|}
    (String.concat ", " (List.map entry packages))

let lock packages =
  let entry (name, version) = Printf.sprintf {|"%s": "%s"|} name version in
  Printf.sprintf {|{ "packages": {%s} }|}
    (String.concat ", " (List.map entry packages))

let test_a_manifest_and_a_lock_bring_a_package_in _ =
  let folder = Sample.folder () in
  Sample.written ~folder ~path:Packages.Manifest.file_name
    (manifest [ ("@dartea/thing", "^1.0.0") ]);
  Sample.written ~folder ~path:Packages.Lock.file_name
    (lock [ ("@dartea/thing", "1.2.0") ]);
  Sample.written ~folder ~path:"src/Main.elm" main;
  vendored ~folder ~package:"@dartea/thing/1.2.0" ~path:"Deep/Thing.elm" thing;
  let sources = sources_of folder in
  assert_equal ~printer:Sample.names [ "Deep.Thing"; "Main" ] (names sources);
  assert_bool "the project did not compile"
    (List.mem "Deep.Thing" (compiled sources))

let test_a_manifest_without_a_lock_is_refused _ =
  let folder = Sample.folder () in
  Sample.written ~folder ~path:Packages.Manifest.file_name
    (manifest [ ("@dartea/thing", "^1.0.0") ]);
  Sample.written ~folder ~path:"src/Main.elm" Sample.starter;
  match refused folder with
  | Missing_lock { file; lock } ->
      assert_equal ~printer:Fun.id Packages.Manifest.file_name
        (Filename.basename file);
      assert_equal ~printer:Fun.id Packages.Lock.file_name lock
  | problem -> unexpected (Project problem)

let test_a_manifest_without_dependencies_needs_no_lock _ =
  let folder = Sample.folder () in
  Sample.written ~folder ~path:Packages.Manifest.file_name (manifest []);
  Sample.written ~folder ~path:"src/Main.elm" Sample.starter;
  assert_equal ~printer:Sample.names [ "Main" ] (names (sources_of folder))

let test_the_manifest_wins_over_elm_json _ =
  let folder = Sample.folder () in
  Sample.written ~folder ~path:"elm.json" (application [ "app" ]);
  Sample.written ~folder ~path:Packages.Manifest.file_name
    {|{ "name": "app", "source-directories": ["vendor"] }|};
  Sample.written ~folder ~path:"app/Main.elm" Sample.starter;
  Sample.written ~folder ~path:"vendor/Deep/Thing.elm" thing;
  assert_equal ~printer:Sample.names [ "Deep.Thing" ]
    (names (sources_of folder))

let entry_of ~entry sources =
  let outcome = Dartea.Compiler.compile_modules ~entry:(Some entry) sources in
  match outcome.errors with
  | [] -> outcome.entry
  | error :: _ ->
      assert_failure
        (Sample.rendered (Reporting.Sources.of_list outcome.sources) error)

let test_entry_is_the_main_declaration _ =
  let folder = Sample.folder () in
  Sample.written ~folder ~path:"src/Main.elm" Sample.starter;
  match entry_of ~entry:"Main" (sources_of folder) with
  | None -> assert_failure "the entry point was not found"
  | Some (entry : Dartea.Entry.t) ->
      assert_equal ~printer:Fun.id "Main" entry.module_name;
      assert_equal ~printer:Fun.id "main" entry.declaration;
      assert_equal ~printer:Typed.Type.show Typed.Type.TStr entry.typ

let test_entry_need_not_be_exposed _ =
  let folder = Sample.folder () in
  Sample.written ~folder ~path:"src/Main.elm"
    {|module Main exposing (answer)


answer : Int
answer =
    1


main : String
main =
    "hi"
|};
  assert_bool "the entry point was not found"
    (Option.is_some (entry_of ~entry:"Main" (sources_of folder)))

let test_module_without_main _ =
  let folder = Sample.folder () in
  Sample.written ~folder ~path:"src/Main.elm" "answer : Int\nanswer =\n    1\n";
  let outcome =
    Dartea.Compiler.compile_modules ~entry:(Some "Main") (sources_of folder)
  in
  match outcome.errors with
  | [ { problem = Project (No_entry { module_name; declaration }); _ } ] ->
      assert_equal ~printer:Fun.id "Main" module_name;
      assert_equal ~printer:Fun.id "main" declaration
  | [] -> assert_failure "the module compiled without an entry point"
  | error :: _ -> unexpected error.problem

let driver = Filename.concat ".." (Filename.concat "bin" "main.exe")

let exit_code = function
  | Unix.WEXITED code -> code
  | Unix.WSIGNALED code | Unix.WSTOPPED code -> -code

let ran ~inside argument =
  let command =
    Printf.sprintf "cd %s && NO_COLOR=1 %s make %s --output=/dev/null 2>&1"
      (Filename.quote inside)
      (Filename.quote (Filename.concat (Sys.getcwd ()) driver))
      (Filename.quote argument)
  in
  let channel = Unix.open_process_in command in
  let printed = Node_runner.read_all channel in
  (exit_code (Unix.close_process_in channel), printed)

let test_empty_folder_stops_the_driver _ =
  let folder = Sample.folder () in
  let code, printed = ran ~inside:folder Filename.current_dir_name in
  assert_equal ~printer:string_of_int 1 code;
  assert_bool printed (Node_runner.contains ~needle:"NO SOURCE FILES" printed)

let test_driver_takes_the_entry_file _ =
  let folder = Sample.folder () in
  Sample.written ~folder ~path:"src/Main.elm" Sample.starter;
  let code, printed = ran ~inside:folder "src/Main.elm" in
  assert_equal ~printer:Fun.id "" printed;
  assert_equal ~printer:string_of_int 0 code

let test_driver_refuses_an_unknown_entry_file _ =
  let folder = Sample.folder () in
  Sample.written ~folder ~path:"src/Main.elm" Sample.starter;
  let code, printed = ran ~inside:folder "src/Nope.elm" in
  assert_equal ~printer:string_of_int 1 code;
  assert_bool printed (Node_runner.contains ~needle:"UNKNOWN ENTRY" printed)

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
    "source_directories_must_be_strings"
    >:: test_source_directories_must_be_strings;
    "source_directory_must_exist" >:: test_source_directory_must_exist;
    "one_module_per_name" >:: test_one_module_per_name;
    "dependency_is_taken_from_disk" >:: test_dependency_is_taken_from_disk;
    "a_package_is_not_a_root" >:: test_a_package_is_not_a_root;
    "our_own_packages_are_skipped" >:: test_our_own_packages_are_skipped;
    "a_missing_package_is_refused" >:: test_a_missing_package_is_refused;
    "a_manifest_and_a_lock_bring_a_package_in"
    >:: test_a_manifest_and_a_lock_bring_a_package_in;
    "a_manifest_without_a_lock_is_refused"
    >:: test_a_manifest_without_a_lock_is_refused;
    "a_manifest_without_dependencies_needs_no_lock"
    >:: test_a_manifest_without_dependencies_needs_no_lock;
    "the_manifest_wins_over_elm_json" >:: test_the_manifest_wins_over_elm_json;
    "entry_is_the_main_declaration" >:: test_entry_is_the_main_declaration;
    "entry_need_not_be_exposed" >:: test_entry_need_not_be_exposed;
    "module_without_main" >:: test_module_without_main;
    "empty_folder_stops_the_driver" >:: test_empty_folder_stops_the_driver;
    "driver_takes_the_entry_file" >:: test_driver_takes_the_entry_file;
    "driver_refuses_an_unknown_entry_file"
    >:: test_driver_refuses_an_unknown_entry_file;
  ]
