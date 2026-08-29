let use_colours () =
  match Sys.getenv_opt "NO_COLOR" with
  | Some _ -> false
  | None -> Unix.isatty Unix.stderr

let print_reports reports =
  let colours = use_colours () in
  List.iter
    (fun report -> prerr_endline (Reporting.Report.to_string ~colours report))
    reports

let refuse ~seen errors =
  print_reports (List.map (Reporting.Sources.report seen) errors);
  exit 1

let save ~path (file : Dartea.Delivery.file) =
  Files.save path (Fpath.v file.path) file.content

let warn ~seen (outcome : Dartea.Compiler.outcome) =
  List.iter
    (fun (module_ : Dartea.Compiler.linkable) ->
      print_reports (List.map (Reporting.Sources.warning seen) module_.warnings))
    outcome.modules

let dev_null = "/dev/null"

let deliver ~path ~seen ~output (outcome : Dartea.Compiler.outcome) =
  if String.equal output dev_null then ()
  else
    let delivery = Dartea.Delivery.for_output output in
    match Dartea.Delivery.produce ~delivery ~output outcome with
    | files -> List.iter (save ~path) files
    | exception Reporting.Error.Found error -> refuse ~seen [ error ]

let compile ~path ~output ~entry sources =
  let outcome = Dartea.Compiler.compile_modules ~entry sources in
  let seen = Reporting.Sources.of_list outcome.sources in
  warn ~seen outcome;
  match outcome.errors with
  | [] -> deliver ~path ~seen ~output outcome
  | errors -> refuse ~seen errors

let entry_in sources path =
  match
    List.find_opt
      (fun (source : Project.Elm_file.t) -> String.equal source.path path)
      sources
  with
  | Some source -> source.name
  | None ->
      refuse ~seen:Reporting.Sources.empty
        [
          Reporting.Error.of_failure
            (Diagnostic.Failure.about (Unknown_entry { path }));
        ]

let load path =
  match Project.Sources.load ~provided:Prelude.packages path with
  | Ok sources -> sources
  | Error failure ->
      refuse ~seen:Reporting.Sources.empty
        [ Reporting.Error.of_failure failure ]

let folder_and_entry target =
  if String.ends_with ~suffix:Project.Elm_file.extension target then
    (Filename.current_dir_name, Some target)
  else (target, None)

let make target output =
  let folder, entry_file = folder_and_entry target in
  let path = Fpath.v folder in
  let sources = load path in
  let entry = Option.map (entry_in (Project.Sources.files sources)) entry_file in
  compile ~path ~output ~entry sources

let count_of picked =
  let count = List.length picked in
  Printf.sprintf "%d package%s" count (if count = 1 then "" else "s")

let install folder =
  match Registry.Install.run (Fpath.v folder) ~say:print_endline
  with
  | picked ->
      print_endline
        (Printf.sprintf "I resolved %s and wrote %s." (count_of picked)
           Packages.Lock.file_name)
  | exception Reporting.Error.Found error ->
      refuse ~seen:Reporting.Sources.empty [ error ]

let target =
  let doc = "The source folder, or a single $(b,.elm) file to start from." in
  Cmdliner.Arg.(
    value
    & pos 0 string Filename.current_dir_name
    & info [] ~docv:"TARGET" ~doc)

let output =
  let doc =
    "Where to write the result. The extension decides what is produced: \
     $(b,.js) writes a script, $(b,.html) writes a page with the script \
     inside, $(b,/dev/null) writes nothing and only checks. Anything else is \
     taken as a folder, and one file per module is written into it."
  in
  Cmdliner.Arg.(
    value & opt string "index.html" & info [ "output" ] ~docv:"FILE" ~doc)

let folder =
  let doc = "The project folder to install into." in
  Cmdliner.Arg.(
    value
    & pos 0 string Filename.current_dir_name
    & info [] ~docv:"FOLDER" ~doc)

let install_command =
  let doc = "Download the packages your project depends on." in
  let man =
    [
      `S Cmdliner.Manpage.s_description;
      `P
        "Reads the dependencies from $(b,dartea.json), works out one \
         version of every package that everyone agrees on, downloads them, \
         and writes the versions it settled on into $(b,dartea.lock).";
    ]
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "install" ~doc ~man)
    Cmdliner.Term.(const install $ folder)

let make_command =
  let doc = "Compile Elm files into JavaScript." in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "make" ~doc)
    Cmdliner.Term.(const make $ target $ output)

let dartea =
  let doc = "an independent compiler for the Elm language" in
  Cmdliner.Cmd.group (Cmdliner.Cmd.info "dartea" ~version:"0.1.0" ~doc)
    [ make_command; install_command ]

let () = exit (Cmdliner.Cmd.eval dartea)
