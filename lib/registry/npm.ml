open Packages

exception Offline of { url : string; problem : string }

type release = {
  version : Version.t;
  dependencies : (string * Version.Range.t) list;
  tarball : string;
  integrity : string;
}

type refusal = { at : Version.t; dependency : string; range : string }
type found = { releases : release list; refusals : refusal list }

type t = { client : Ezcurl.t; known : (string, found) Hashtbl.t }

let home = "https://registry.npmjs.org/"
let metadata_accept = "application/vnd.npm.install-v1+json"

let escape package = String.concat "%2f" (String.split_on_char '/' package)
let address package = home ^ escape package

type dist = { tarball : string; integrity : string }
[@@deriving of_yojson { strict = false }]

type published = {
  dependencies : (string * Yojson.Safe.t) list;
      [@default []] [@of_yojson Json.pairs Result.ok]
  dist : dist;
}
[@@deriving of_yojson { strict = false }]

type document = {
  versions : (string * Yojson.Safe.t) list;
      [@default []] [@of_yojson Json.pairs Result.ok]
}
[@@deriving of_yojson { strict = false }]

let releases content =
  let range_of (name, written) =
    match written with
    | `String range ->
        Option.to_result ~none:(name, range)
          (Option.map
             (fun held -> (name, held))
             (Version.Range.of_string range))
    | _ -> Error (name, Yojson.Safe.to_string written)
  in
  let assemble version (published : published) =
    List.fold_right
      (fun row gathered ->
        Result.bind (range_of row) (fun range ->
            Result.map (fun rest -> range :: rest) gathered))
      published.dependencies (Ok [])
    |> Result.map (fun dependencies ->
           {
             version;
             dependencies;
             tarball = published.dist.tarball;
             integrity = published.dist.integrity;
           })
    |> Result.map_error (fun (dependency, range) ->
           { at = version; dependency; range })
  in
  let release_of (written, held) =
    match (Version.of_string written, published_of_yojson held) with
    | Some version, Ok published -> Some (assemble version published)
    | _ -> None
  in
  let outcomes =
    match Yojson.Safe.from_string content with
    | json ->
        Result.fold
          ~ok:(fun document -> List.filter_map release_of document.versions)
          ~error:(Fun.const []) (document_of_yojson json)
    | exception Yojson.Json_error _ -> []
  in
  {
    releases = List.filter_map Result.to_option outcomes;
    refusals =
      List.filter_map
        (function Error refusal -> Some refusal | Ok _ -> None)
        outcomes;
  }

let nothing = { releases = []; refusals = [] }
let anything = "*/*"

let payload registry url =
  match Fetch.get registry.client ~accept:anything url with
  | Fetch.Found content -> Some content
  | Fetch.Absent -> None
  | Fetch.Broken problem -> raise (Offline { url; problem })

let remember registry package found =
  Hashtbl.replace registry.known package found;
  found

let download registry package =
  let url = address package in
  match Fetch.get registry.client ~accept:metadata_accept url with
  | Fetch.Found content -> remember registry package (releases content)
  | Fetch.Absent -> remember registry package nothing
  | Fetch.Broken problem -> raise (Offline { url; problem })

let load registry package =
  match Hashtbl.find_opt registry.known package with
  | Some found -> found
  | None -> download registry package

let at registry (pick : Pick.t) =
  List.find_opt
    (fun release -> Version.compare release.version pick.version = 0)
    (load registry pick.package).releases

let dependencies registry package version =
  match at registry { package; version } with
  | Some release -> release.dependencies
  | None -> []

let versions registry package =
  List.map (fun release -> release.version) (load registry package).releases

let newest one other = Version.compare other.at one.at

let refusals registry package =
  List.sort newest (load registry package).refusals

let view registry =
  {
    Solver.versions = versions registry;
    dependencies = dependencies registry;
  }

let open_ client = { client; known = Hashtbl.create 16 }
