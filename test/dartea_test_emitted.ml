open OUnit2

let root = Filename.concat ".." "playgrounds"
let goldens = "emitted"
let prelude_folder = "prelude"
let suffix = "." ^ Dartea.Compiler.extension
let read path = In_channel.with_open_bin path In_channel.input_all
let promoting_into = Sys.getenv_opt "DARTEA_PROMOTE_EMITTED"
let names_printer names = String.concat ", " names
let named modules = List.sort String.compare (List.map fst modules)

let golden ~folder module_name =
  Filename.concat (Filename.concat goldens folder) (module_name ^ suffix)

let promoted ~folder ~module_name content =
  Option.iter
    (fun directory ->
      let target = Filename.concat directory folder in
      if not (Sys.file_exists target) then Sys.mkdir target 0o755;
      Out_channel.with_open_bin
        (Filename.concat target (module_name ^ suffix))
        (fun out -> Out_channel.output_string out content))
    promoting_into

let refused sources errors =
  assert_failure
    (String.concat "\n"
       (List.map
          (fun error ->
            Reporting.Report.to_string ~colours:false
              (Reporting.Sources.report sources error))
          errors))

let emitted (outcome : Dartea.Compiler.outcome) =
  match outcome.errors with
  | [] ->
      List.map
        (fun (compiled : Dartea.Compiler.compiled) ->
          (compiled.module_name, compiled.source))
        outcome.output
  | errors -> refused (Reporting.Sources.of_list outcome.sources) errors

let compiled_in folder =
  Eio_main.run @@ fun env ->
  match File_loader.Files.load Eio.Path.(Eio.Stdenv.fs env / folder) with
  | Ok sources -> Dartea.Compiler.compile_modules sources
  | Error error -> refused Reporting.Sources.empty [ error ]

let prelude_emitted = lazy (emitted (Dartea.Compiler.compile_modules []))

let listed folder =
  let path = Filename.concat goldens folder in
  if Sys.file_exists path then
    Sys.readdir path |> Array.to_list
    |> List.filter (fun file -> Filename.check_suffix file suffix)
    |> List.map Filename.remove_extension
    |> List.sort String.compare
  else []

let same_as_golden ~folder (module_name, source) =
  let path = golden ~folder module_name in
  assert_bool (Printf.sprintf "%s is missing" path) (Sys.file_exists path);
  assert_equal ~printer:Fun.id ~msg:path (read path) source

let same_as_goldens ~folder modules =
  List.iter
    (fun (module_name, source) -> promoted ~folder ~module_name source)
    modules;
  assert_equal ~printer:names_printer ~msg:folder (listed folder) (named modules);
  List.iter (same_as_golden ~folder) modules

let test_prelude _ =
  same_as_goldens ~folder:prelude_folder (Lazy.force prelude_emitted)

let playgrounds =
  Sys.readdir root |> Array.to_list
  |> List.filter (fun entry ->
         let path = Filename.concat root entry in
         Sys.is_directory path
         && Array.exists
              (fun file -> Filename.check_suffix file ".elm")
              (Sys.readdir path))
  |> List.sort String.compare

let unchanged_prelude ~folder produced =
  List.iter
    (fun (module_name, source) ->
      match List.assoc_opt module_name (Lazy.force prelude_emitted) with
      | None -> ()
      | Some expected ->
          assert_equal ~printer:Fun.id
            ~msg:
              (Printf.sprintf "%s in %s drifted from the prelude" module_name
                 folder)
            expected source)
    produced

let own produced =
  let prelude = Lazy.force prelude_emitted in
  List.filter
    (fun (module_name, _) -> not (List.mem_assoc module_name prelude))
    produced

let test_playground folder =
  folder >:: fun _ ->
  let produced = emitted (compiled_in (Filename.concat root folder)) in
  unchanged_prelude ~folder produced;
  same_as_goldens ~folder (own produced)

let suite = ("prelude" >:: test_prelude) :: List.map test_playground playgrounds
