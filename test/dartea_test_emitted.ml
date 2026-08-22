open OUnit2

let root = Filename.concat ".." "playgrounds"
let goldens = "emitted"
let prelude_folder = "prelude"
let suffix = "." ^ Dartea.Compiler.extension
let read path = In_channel.with_open_bin path In_channel.input_all
let promoting_into = Sys.getenv_opt "DARTEA_PROMOTE_EMITTED"

let promoted ~folder ~module_name content =
  Option.iter
    (fun directory ->
      let target = Filename.concat directory folder in
      if not (Sys.file_exists target) then Sys.mkdir target 0o755;
      Out_channel.with_open_bin
        (Filename.concat target (module_name ^ suffix))
        (fun out -> Out_channel.output_string out content))
    promoting_into

let emitted (outcome : Dartea.Compiler.outcome) =
  match outcome.errors with
  | [] ->
      List.map
        (fun (compiled : Dartea.Compiler.compiled) ->
          (compiled.module_name, compiled.source))
        outcome.output
  | errors ->
      let sources = Reporting.Sources.of_list outcome.sources in
      assert_failure
        (String.concat "\n"
           (List.map
              (fun error ->
                Reporting.Report.to_string ~colours:false
                  (Reporting.Sources.report sources error))
              errors))

let compiled_in folder =
  Eio_main.run @@ fun env ->
  Eio.Path.(Eio.Stdenv.fs env / folder)
  |> File_loader.Files.current_folder |> Dartea.Compiler.compile_modules

let prelude_emitted = lazy (emitted (Dartea.Compiler.compile_modules []))

let listed folder =
  let path = Filename.concat goldens folder in
  if Sys.file_exists path then
    Sys.readdir path |> Array.to_list
    |> List.filter (fun file -> Filename.check_suffix file suffix)
    |> List.map Filename.remove_extension
    |> List.sort String.compare
  else []

let names_printer names = String.concat ", " names

let same_as_golden ~folder (module_name, source) =
  let path = Filename.concat (Filename.concat goldens folder) (module_name ^ suffix) in
  assert_bool (Printf.sprintf "%s is missing" path) (Sys.file_exists path);
  assert_equal ~printer:Fun.id ~msg:path (read path) source

let test_prelude _ =
  let produced = Lazy.force prelude_emitted in
  List.iter
    (fun (module_name, source) ->
      promoted ~folder:prelude_folder ~module_name source)
    produced;
  assert_equal ~printer:names_printer ~msg:prelude_folder (listed prelude_folder)
    (List.map fst produced |> List.sort String.compare);
  List.iter (same_as_golden ~folder:prelude_folder) produced

let playgrounds =
  Sys.readdir root |> Array.to_list
  |> List.filter (fun entry ->
         let path = Filename.concat root entry in
         Sys.is_directory path
         && Array.exists
              (fun file -> Filename.check_suffix file ".elm")
              (Sys.readdir path))
  |> List.sort String.compare

let test_playground folder =
  folder >:: fun _ ->
  let produced = emitted (compiled_in (Filename.concat root folder)) in
  let prelude = Lazy.force prelude_emitted in
  let own =
    List.filter (fun (module_name, _) -> not (List.mem_assoc module_name prelude)) produced
  in
  List.iter (fun (module_name, source) -> promoted ~folder ~module_name source) own;
  assert_equal ~printer:names_printer ~msg:folder (listed folder)
    (List.map fst own |> List.sort String.compare);
  List.iter (same_as_golden ~folder) own;
  List.iter
    (fun (module_name, source) ->
      match List.assoc_opt module_name prelude with
      | None -> ()
      | Some expected ->
          assert_equal ~printer:Fun.id
            ~msg:(Printf.sprintf "%s in %s drifted from the prelude" module_name folder)
            expected source)
    produced

let suite = ("prelude" >:: test_prelude) :: List.map test_playground playgrounds
