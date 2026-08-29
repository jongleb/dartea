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

let solved registry ~file (manifest : Manifest.t) =
  match Solver.solved (Npm.view registry) manifest.dependencies with
  | Ok picked -> picked
  | Error (Solver.Unknown_package { package; asked_by }) ->
      Diagnostic.Failure.raise_project
        (Unknown_package { file; package; asked_by })
  | Error (Solver.No_version { package; asked }) ->
      unusable registry ~file package asked

let refuse ~file (pick : Pick.t) problem =
  Diagnostic.Failure.raise_project
    (Bad_tarball
       {
         file;
         package = pick.package;
         version = Version.show pick.version;
         problem;
       })

let stored registry root ~file pick (release : Npm.release) =
  match Npm.payload registry release.tarball with
  | Some content -> Store.kept root pick ~integrity:release.integrity content
  | None -> refuse ~file pick "the registry no longer serves this tarball"

let installing registry root ~file ~say pick =
  say ("  downloading " ^ Pick.shown pick);
  match Npm.at registry pick with
  | Some release -> stored registry root ~file pick release
  | None ->
      Diagnostic.Failure.raise_project
        (Unknown_package
           { file; package = pick.package; asked_by = "the registry" })

let taken registry root ~file ~say pick =
  if Files.Dir.is_directory root (Vendor.at pick) then
    say ("  keeping " ^ Pick.shown pick)
  else installing registry root ~file ~say pick

let agrees picks (package, range) =
  match Pick.found package picks with
  | Some (pick : Pick.t) ->
      Interval.holds (Interval.of_range range) pick.version
  | None -> false

let covering (manifest : Manifest.t) (lock : Lock.t) =
  if List.for_all (agrees lock.packages) manifest.dependencies then
    Some lock.packages
  else None

let pinned root manifest =
  Option.bind (Lock.of_folder root) (covering manifest)

let performed registry root ~say manifest =
  let file = manifest.Manifest.file in
  let picked =
    match pinned root manifest with
    | Some packages -> packages
    | None -> solved registry ~file manifest
  in
  List.iter (taken registry root ~file ~say) picked;
  Lock.saved root picked;
  picked

let carried root ~say manifest =
  Ezcurl.with_client (fun client ->
      performed (Npm.opened client) root ~say manifest)

let resolved root ~say (manifest : Manifest.t) =
  match carried root ~say manifest with
  | picked -> picked
  | exception Npm.Offline { url; problem } ->
      Diagnostic.Failure.raise_project
        (Offline { file = manifest.file; url; problem })
  | exception Store.Broken { package; version; problem } ->
      Diagnostic.Failure.raise_project
        (Bad_tarball { file = manifest.file; package; version; problem })

let run root ~say =
  match Manifest.of_folder root with
  | Some manifest -> resolved root ~say manifest
  | None ->
      Diagnostic.Failure.raise_project
        (Missing_manifest { file = Manifest.file_name })
