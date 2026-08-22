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

let loaded folder =
  Eio_main.run @@ fun env ->
  File_loader.Files.current_folder Eio.Path.(Eio.Stdenv.fs env / folder)

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

let thing = "module Deep.Thing exposing (answer)\n\n\nanswer : Int\nanswer =\n    42\n"

let main =
  "module Main exposing (main)\n\nimport Deep.Thing\n\n\nmain : Int\nmain =\n    Deep.Thing.answer + 1\n"

let compiled sources =
  let outcome = Dartea.Compiler.compile_modules sources in
  match outcome.errors with
  | [] ->
      sorted
        (List.map
           (fun (module_ : Dartea.Compiler.compiled) -> module_.module_name)
           outcome.output)
  | error :: _ ->
      let seen = Reporting.Sources.of_list outcome.sources in
      assert_failure
        (Reporting.Report.to_string ~colours:false
           (Reporting.Sources.report seen error))

let test_source_folder _ =
  let folder = Filename.temp_dir "dartea" "" in
  written ~folder ~path:"src/Main.elm" main;
  written ~folder ~path:"src/Deep/Thing.elm" thing;
  let sources = loaded folder in
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
  let sources = loaded folder in
  assert_equal ~printer:names_printer [ "Deep.Thing"; "Main" ] (names sources);
  assert_equal ~printer:names_printer [ "Deep/Thing.elm"; "Main.elm" ] (paths sources);
  assert_bool "the project did not compile"
    (List.mem "Deep.Thing" (compiled sources))

let test_hidden_folders_are_skipped _ =
  let folder = Filename.temp_dir "dartea" "" in
  written ~folder ~path:"Main.elm" "main : Int\nmain =\n    1\n";
  written ~folder ~path:".hidden/Other.elm" "other : Int\nother =\n    2\n";
  assert_equal ~printer:names_printer [ "Main" ] (names (loaded folder))

let mismatched (error : Reporting.Error.t) =
  match error.problem with
  | Syntax (Module_name_mismatch { expected }) ->
      assert_equal ~printer:Fun.id "Deep.Other" expected
  | problem -> assert_failure (Reporting.Error.show_problem problem)

let test_name_must_match_the_path _ =
  let folder = Filename.temp_dir "dartea" "" in
  written ~folder ~path:"src/Deep/Other.elm" thing;
  match Dartea.Compiler.compile_modules (loaded folder) with
  | outcome -> (
      match outcome.errors with
      | [] -> assert_failure "the mismatch compiled without an error"
      | error :: _ -> mismatched error)
  | exception Reporting.Error.Found error -> mismatched error

let test_empty_folder_is_empty _ =
  let folder = Filename.temp_dir "dartea" "" in
  assert_equal ~printer:names_printer [] (names (loaded folder))

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
    "empty_folder_is_empty" >:: test_empty_folder_is_empty;
    "empty_folder_stops_the_driver" >:: test_empty_folder_stops_the_driver;
  ]
