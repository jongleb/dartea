open Packages

let unreadable ~file package (refusal : Npm.refusal) =
  Diagnostic.Failure.raise_project
    (Bad_range
       {
         file;
         package;
         version = Version.show refusal.at;
         dependency = refusal.dependency;
         range = refusal.range;
       })

let unusable registry ~file package asked =
  match Npm.refusals registry package with
  | refusal :: _ -> unreadable ~file package refusal
  | [] -> Diagnostic.Failure.raise_project (No_version { file; package; asked })

let solve registry ~file (manifest : Manifest.t) =
  match Solver.solve (Npm.view registry) manifest.dependencies with
  | Ok picks -> picks
  | Error (Solver.Unknown_package { package; asked_by }) ->
      Diagnostic.Failure.raise_project
        (Unknown_package { file; package; asked_by })
  | Error (Solver.No_version { package; asked }) ->
      unusable registry ~file package asked

let store registry root pick (release : Npm.release) =
  match Npm.payload registry release.tarball with
  | Some content -> Store.kept root pick ~integrity:release.integrity content
  | None -> Store.refuse pick "the registry no longer serves this tarball"

let install registry root ~file pick =
  match Npm.at registry pick with
  | Some release -> store registry root pick release
  | None ->
      Diagnostic.Failure.raise_project
        (Unknown_package
           { file; package = pick.package; asked_by = "the registry" })

let taken root ~say fetch pick =
  if Files.is_directory root (Pick.folder pick) then
    say ("  keeping " ^ Pick.shown pick)
  else (
    say ("  downloading " ^ Pick.shown pick);
    fetch pick)

let agrees picks (package, range) =
  match Pick.found package picks with
  | Some (pick : Pick.t) ->
      Version.Interval.holds (Version.Interval.of_range range) pick.version
  | None -> false

let valid_picks (manifest : Manifest.t) (lock : Lock.t) =
  if List.for_all (agrees lock.packages) manifest.dependencies then
    Some lock.packages
  else None

let picks_from_lock root manifest =
  Option.bind (Lock.of_folder root) (valid_picks manifest)

let perform registry root ~say manifest =
  let file = manifest.Manifest.file in
  let picks =
    match picks_from_lock root manifest with
    | Some packages -> packages
    | None -> solve registry ~file manifest
  in
  List.iter (taken root ~say (install registry root ~file)) picks;
  Lock.save root picks;
  picks

let guarded ~file work =
  match Ezcurl.with_client work with
  | outcome -> outcome
  | exception Fetch.Offline { url; problem } ->
      Diagnostic.Failure.raise_project (Offline { file; url; problem })
  | exception Store.Broken { package; version; problem } ->
      Diagnostic.Failure.raise_project
        (Bad_tarball { file; package; version; problem })

let no_manifest () =
  Diagnostic.Failure.raise_project
    (Missing_manifest { file = Manifest.file_name })

type t =
  | Locked of Pick.t list
  | Fetched of { file : string; packages : Pick.t list }

let run root ~provided ~say =
  let from_elm client ~file (pick : Pick.t) =
    let unpack (endpoint : Elm_registry.endpoint) content =
      match Zipball.kept root pick ~hash:endpoint.hash content with
      | Zipball.Placed -> ()
      | Zipball.Kernel ->
          Diagnostic.Failure.raise_project
            (Kernel_package
               {
                 file;
                 package = pick.package;
                 version = Version.show pick.version;
               })
    in
    let download (endpoint : Elm_registry.endpoint) =
      match Fetch.body client ~accept:Fetch.anything endpoint.url with
      | Some content -> unpack endpoint content
      | None -> Store.refuse pick ("GitHub no longer serves " ^ endpoint.url)
    in
    match Elm_registry.endpoint client pick with
    | Some endpoint -> download endpoint
    | None ->
        Diagnostic.Failure.raise_project
          (Unknown_package { file; package = pick.package; asked_by = file })
  in
  let fetch_all = function
    | None -> no_manifest ()
    | Some (outline : Project.Outline.t) ->
        let file = outline.file in
        let packages =
          List.filter
            (fun (pick : Pick.t) -> not (List.mem pick.package provided))
            outline.dependencies
        in
        guarded ~file (fun client ->
            List.iter (taken root ~say (from_elm client ~file)) packages);
        Fetched { file; packages }
  in
  match Manifest.of_folder root with
  | Some manifest ->
      Locked
        (guarded ~file:manifest.file (fun client ->
             perform (Npm.open_ client) root ~say manifest))
  | None -> fetch_all (Project.Outline.of_elm_json root)

let add root ~package ~say =
  let manifest =
    match Manifest.of_folder root with
    | Some manifest -> manifest
    | None -> no_manifest ()
  in
  let file = manifest.file in
  guarded ~file (fun client ->
      let registry = Npm.open_ client in
      if List.mem_assoc package manifest.dependencies then
        perform registry root ~say manifest
      else
        match
          List.sort
            (fun one other -> Version.compare other one)
            (Npm.versions registry package)
        with
        | [] ->
            Diagnostic.Failure.raise_project
              (Unknown_package { file; package; asked_by = "dartea install" })
        | newest :: _ ->
            let dependencies =
              manifest.dependencies @ [ (package, Version.Range.Upward newest) ]
            in
            let picks = perform registry root ~say { manifest with dependencies } in
            Manifest.save_dependencies root dependencies;
            picks)
