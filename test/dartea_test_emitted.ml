open OUnit2

let goldens = "emitted"
let prelude_folder = "prelude"
let promoting_into = Sys.getenv_opt "DARTEA_PROMOTE_EMITTED"
let named modules = List.sort String.compare (List.map fst modules)

let golden ~folder module_name =
  Filename.concat
    (Filename.concat goldens folder)
    (Codegen_js.Of_optimized.module_file module_name)

let promoted ~folder ~module_name content =
  Option.iter
    (fun directory ->
      let target = Filename.concat directory folder in
      if not (Sys.file_exists target) then Sys.mkdir target 0o755;
      Out_channel.with_open_bin
        (Filename.concat target
           (Codegen_js.Of_optimized.module_file module_name))
        (fun out -> Out_channel.output_string out content))
    promoting_into

let emitted = Sample.emitted
let compiled_in = Sample.compiled_in
let refused = Sample.refused

let probe = "Prelude"

let probe_source =
  let imports =
    Prelude.all
    |> List.filter (fun module_ -> not (Prelude.imported_by_default module_))
    |> List.map (fun module_ -> "import " ^ Prelude.name module_)
    |> String.concat "\n"
  in
  Printf.sprintf
    "module %s exposing (nothing)\n\n%s\n\nnothing : Int\nnothing =\n    0\n"
    probe imports

let prelude_emitted =
  lazy
    (Dartea.Compiler.compile_modules ~entry:None
       [ Project.Elm_file.of_path ~path:(probe ^ ".elm") probe_source ]
    |> emitted
    |> List.filter (fun (module_name, _) ->
           not (String.equal module_name probe)))

let listed folder =
  let path = Filename.concat goldens folder in
  if Sys.file_exists path then
    Sys.readdir path |> Array.to_list
    |> List.filter (fun file ->
           Filename.check_suffix file Codegen_js.Of_optimized.module_suffix)
    |> List.map Filename.remove_extension
    |> List.sort String.compare
  else []

let same_as_golden ~folder (module_name, source) =
  let path = golden ~folder module_name in
  assert_bool (Printf.sprintf "%s is missing" path) (Sys.file_exists path);
  assert_equal ~printer:Fun.id ~msg:path (Sample.read path) source

let same_as_goldens ~folder modules =
  List.iter
    (fun (module_name, source) -> promoted ~folder ~module_name source)
    modules;
  assert_equal ~printer:Sample.names ~msg:folder (listed folder)
    (named modules);
  List.iter (same_as_golden ~folder) modules

let test_prelude _ =
  same_as_goldens ~folder:prelude_folder (Lazy.force prelude_emitted)

let playgrounds = Sample.playgrounds

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
  let produced = emitted (compiled_in (Filename.concat Sample.playground_root folder)) in
  unchanged_prelude ~folder produced;
  same_as_goldens ~folder (own produced)

let suite = ("prelude" >:: test_prelude) :: List.map test_playground playgrounds
