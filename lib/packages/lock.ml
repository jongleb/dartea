type t = { file : string; packages : Pick.t list }

let file_name = "dartea.lock"
let path = Fpath.v file_name
let field = "packages"

type written = {
  packages : (string * Version.t) list; [@of_yojson Json.pairs Version.of_yojson]
}
[@@deriving of_yojson { strict = false }]

let decode ~file content =
  {
    file;
    packages =
      List.map Pick.of_pair (Json.decode ~file written_of_yojson content).packages;
  }

let of_folder root = Json.load root path decode

let shown packages =
  let row (pick : Pick.t) =
    (pick.package, `String (Version.show pick.version))
  in
  let in_order = List.sort Pick.by_name packages in
  let rows = `Assoc [ (field, `Assoc (List.map row in_order)) ] in
  Yojson.Safe.pretty_to_string rows ^ "\n"

let save root packages = Files.save root path (shown packages)
