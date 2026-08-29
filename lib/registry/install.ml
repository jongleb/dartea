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

let install registry root ~file ~say pick =
  say ("  downloading " ^ Pick.shown pick);
  match Npm.at registry pick with
  | Some release -> store registry root pick release
  | None ->
      Diagnostic.Failure.raise_project
        (Unknown_package
           { file; package = pick.package; asked_by = "the registry" })

let taken registry root ~file ~say pick =
  if Files.is_directory root (Pick.folder pick) then
    say ("  keeping " ^ Pick.shown pick)
  else install registry root ~file ~say pick

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
  List.iter (taken registry root ~file ~say) picks;
  Lock.save root picks;
  picks

let with_registry root ~say manifest =
  Ezcurl.with_client (fun client ->
      perform (Npm.open_ client) root ~say manifest)

let resolve root ~say (manifest : Manifest.t) =
  match with_registry root ~say manifest with
  | picks -> picks
  | exception Npm.Offline { url; problem } ->
      Diagnostic.Failure.raise_project
        (Offline { file = manifest.file; url; problem })
  | exception Store.Broken { package; version; problem } ->
      Diagnostic.Failure.raise_project
        (Bad_tarball { file = manifest.file; package; version; problem })

let run root ~say =
  match Manifest.of_folder root with
  | Some manifest -> resolve root ~say manifest
  | None ->
      Diagnostic.Failure.raise_project
        (Missing_manifest { file = Manifest.file_name })
