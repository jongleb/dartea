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

let text rows field =
  match List.assoc_opt field rows with
  | Some (`String written) -> Some written
  | _ -> None

let parse_range name range =
  match Version.Range.of_string range with
  | Some held -> Ok (name, held)
  | None -> Error (name, range)

let range_of_row (name, written) =
  match written with
  | `String range -> parse_range name range
  | _ -> Error (name, Yojson.Safe.to_string written)

let rec ranged_all = function
  | [] -> Ok []
  | (name, held) :: rest ->
      Result.bind (range_of_row (name, held)) (fun range ->
          Result.map (fun others -> range :: others) (ranged_all rest))

let dependencies_of rows =
  match List.assoc_opt "dependencies" rows with
  | None -> Ok []
  | Some (`Assoc packages) -> ranged_all packages
  | Some held -> Error ("dependencies", Yojson.Safe.to_string held)

let dist_of rows =
  match List.assoc_opt "dist" rows with
  | Some (`Assoc dist) -> Some dist
  | _ -> None

let built version dependencies dist =
  match (text dist "tarball", text dist "integrity") with
  | Some tarball, Some integrity ->
      Some (Ok { version; dependencies; tarball; integrity })
  | _ -> None

let assemble version rows =
  match (dependencies_of rows, dist_of rows) with
  | Ok dependencies, Some dist -> built version dependencies dist
  | Error (dependency, range), _ ->
      Some (Error { at = version; dependency; range })
  | Ok _, None -> None

let release_of (written, held) =
  match (Version.of_string written, held) with
  | Some version, `Assoc rows -> assemble version rows
  | _ -> None

let refusal_of = function Error refusal -> Some refusal | Ok _ -> None

let sift outcomes =
  {
    releases = List.filter_map Result.to_option outcomes;
    refusals = List.filter_map refusal_of outcomes;
  }

let nothing = { releases = []; refusals = [] }

let releases_of_rows rows =
  match List.assoc_opt "versions" rows with
  | Some (`Assoc releases) -> sift (List.filter_map release_of releases)
  | _ -> nothing

let releases content =
  match Yojson.Safe.from_string content with
  | `Assoc rows -> releases_of_rows rows
  | _ -> nothing
  | exception Yojson.Json_error _ -> nothing

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
