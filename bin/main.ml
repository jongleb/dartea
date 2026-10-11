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

let refuse_failure failure =
  refuse ~seen:Reporting.Sources.empty [ Reporting.Error.of_failure failure ]

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
  | None -> refuse_failure (Diagnostic.Failure.about (Unknown_entry { path }))

let load path =
  match Project.Sources.load ~provided:Prelude.packages path with
  | Ok sources -> sources
  | Error failure -> refuse_failure failure

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

let install package =
  let root = Fpath.v Filename.current_dir_name in
  let resolve () =
    match package with
    | Some package -> Registry.Install.add root ~package ~say:print_endline
    | None -> Registry.Install.run root ~say:print_endline
  in
  match resolve () with
  | picked ->
      print_endline
        (Printf.sprintf "I resolved %s and wrote %s." (count_of picked)
           Packages.Lock.file_name)
  | exception Diagnostic.Failure.Found failure -> refuse_failure failure

let init folder =
  match Project.Scaffold.create (Fpath.v folder) with
  | written ->
      List.iter
        (fun path ->
          print_endline
            (Printf.sprintf "I wrote %s." (Fpath.to_string Fpath.(v folder // path))))
        written;
      print_endline
        "Run `dartea make src/Main.elm --output=index.html` in the project \
         folder, then open index.html in a browser."
  | exception Diagnostic.Failure.Found failure -> refuse_failure failure

let publish folder mode =
  let path = Fpath.v folder in
  let sources = load path in
  compile ~path ~output:dev_null ~entry:None sources;
  let modules =
    List.filter_map Project.Elm_file.written (Project.Sources.files sources)
  in
  match Registry.Publish.run path ~modules mode with
  | Published listing ->
      print_endline
        (Printf.sprintf "I published %s %s." listing.name
           (Packages.Version.show listing.version))
  | Rehearsed listing ->
      print_endline
        (Printf.sprintf "%s %s is ready to publish. Nothing was sent."
           listing.name
           (Packages.Version.show listing.version))
  | Npm_failed code -> exit code
  | exception Diagnostic.Failure.Found failure -> refuse_failure failure

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

let folder doc =
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
      `P
        "With a $(i,PACKAGE), adds its newest version to $(b,dartea.json) \
         first, as $(b,^)$(i,version), but only if it fits with the rest.";
    ]
  in
  let package =
    Cmdliner.Arg.(
      value
      & pos 0 (some string) None
      & info [] ~docv:"PACKAGE" ~doc:"A package to add, like @someone/charts.")
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "install" ~doc ~man)
    Cmdliner.Term.(const install $ package)

let init_command =
  let doc = "Start a new project." in
  let man =
    [
      `S Cmdliner.Manpage.s_description;
      `P
        "Writes $(b,dartea.json) and $(b,src/Main.elm) with a small counter \
         application, ready for $(b,dartea make).";
    ]
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "init" ~doc ~man)
    Cmdliner.Term.(const init $ folder "The folder to start the project in.")

let publish_command =
  let doc = "Publish a package to npm." in
  let man =
    [
      `S Cmdliner.Manpage.s_description;
      `P
        "Checks that the package compiles and that every module in \
         $(b,exposed-modules) exists, then hands $(b,dartea.json), \
         $(b,src) and a generated $(b,package.json) to $(b,npm publish). \
         Log in with $(b,npm login) first.";
    ]
  in
  let mode =
    Cmdliner.Arg.(
      value
      & vflag Registry.Publish.Publish
          [
            ( Registry.Publish.Rehearse,
              info [ "dry-run" ]
                ~doc:"Do everything except sending the package to npm." );
          ])
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "publish" ~doc ~man)
    Cmdliner.Term.(
      const publish $ folder "The package folder to publish." $ mode)

let make_command =
  let doc = "Compile Elm files into JavaScript." in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "make" ~doc)
    Cmdliner.Term.(const make $ target $ output)

let dartea =
  let doc = "an independent compiler for the Elm language" in
  Cmdliner.Cmd.group (Cmdliner.Cmd.info "dartea" ~version:"0.2.0" ~doc)
    [ init_command; make_command; install_command; publish_command ]

let () = exit (Cmdliner.Cmd.eval dartea)
