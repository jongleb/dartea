type t = { file : string; packages : Pick.t list }

let file_name = "dartea.lock"
let path = Files.Relative.of_string file_name
let field = "packages"

let picked ~file (package, held) =
  { Pick.package; version = Json.version ~file ~field held }

let decoded ~file content =
  match List.assoc_opt field (Json.rows_of ~file content) with
  | None -> Json.missing ~file ~field
  | Some (`Assoc packages) ->
      { file; packages = List.map (picked ~file) packages }
  | Some _ -> Json.bad ~file ~field "an object of packages and versions"

let of_folder root = Json.loaded root path decoded

let shown packages =
  let row (pick : Pick.t) =
    (pick.package, `String (Version.show pick.version))
  in
  let sorted = List.sort Pick.by_name packages in
  let rows = `Assoc [ (field, `Assoc (List.map row sorted)) ] in
  Yojson.Safe.pretty_to_string rows ^ "\n"

let saved root packages = Files.Dir.saved root path (shown packages)
