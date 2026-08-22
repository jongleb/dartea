let colours_wanted () =
  match Sys.getenv_opt "NO_COLOR" with
  | Some _ -> false
  | None -> Unix.isatty Unix.stderr

let printed reports =
  let colours = colours_wanted () in
  List.iter
    (fun report -> prerr_endline (Reporting.Report.to_string ~colours report))
    reports

let refused ~seen errors =
  printed (List.map (Reporting.Sources.report seen) errors);
  exit 1

let saved ~path (file : Dartea.Delivery.file) =
  Files.Dir.saved path (Files.Relative.of_string file.path) file.content

let warned ~seen (outcome : Dartea.Compiler.outcome) =
  List.iter
    (fun (module_ : Dartea.Compiler.compiled) ->
      printed (List.map (Reporting.Sources.warning seen) module_.warnings))
    outcome.output

let delivered ~path ~seen ~delivery (outcome : Dartea.Compiler.outcome) =
  let module Delivery = (val delivery : Dartea.Delivery.S) in
  match Delivery.files ~entry:outcome.entry outcome.output with
  | files -> List.iter (saved ~path) files
  | exception Reporting.Error.Found error -> refused ~seen [ error ]

let compiled ~path ~delivery ~entry sources =
  let outcome = Dartea.Compiler.compile_modules ~entry sources in
  let seen = Reporting.Sources.of_list outcome.sources in
  warned ~seen outcome;
  match outcome.errors with
  | [] -> delivered ~path ~seen ~delivery outcome
  | errors -> refused ~seen errors

let entry_in sources path =
  match
    List.find_opt
      (fun (source : Project.Elm_file.t) -> String.equal source.path path)
      sources
  with
  | Some source -> source.name
  | None ->
      refused ~seen:Reporting.Sources.empty
        [ Reporting.Error.project (Unknown_entry { path }) ]

let loaded path =
  match Project.Sources.load path with
  | Ok sources -> sources
  | Error error -> refused ~seen:Reporting.Sources.empty [ error ]

type request = { delivery : string option; target : string }

let dashes = "--"
let flag argument = String.starts_with ~prefix:dashes argument

let without_dashes argument =
  String.sub argument (String.length dashes)
    (String.length argument - String.length dashes)

let requested arguments =
  let flags, targets = List.partition flag arguments in
  {
    delivery =
      (match List.rev flags with
      | [] -> None
      | last :: _ -> Some (without_dashes last));
    target =
      (match targets with
      | [] -> Filename.current_dir_name
      | first :: _ -> first);
  }

let chosen = function
  | None -> Dartea.Delivery.default
  | Some name -> (
      match Dartea.Delivery.find name with
      | found -> found
      | exception Reporting.Error.Found error ->
          refused ~seen:Reporting.Sources.empty [ error ])

let folder_and_entry target =
  if String.ends_with ~suffix:Project.Elm_file.extension target then
    (Filename.current_dir_name, Some target)
  else (target, None)

let ran env request =
  let delivery = chosen request.delivery in
  let folder, entry_file = folder_and_entry request.target in
  let path = Eio.Path.(Eio.Stdenv.fs env / folder) in
  let sources = loaded path in
  let entry = Option.map (entry_in sources) entry_file in
  compiled ~path ~delivery ~entry sources

let () =
  Eio_main.run @@ fun env ->
  ran env (requested (List.tl (Array.to_list Sys.argv)))
