type t = { file : string; packages : Pick.t list }

let file_name = "dartea.lock"
let path = Fpath.v file_name

type written = {
  packages : (string * Version.t) list;
      [@of_yojson Json.pairs Version.of_yojson]
      [@to_yojson Json.object_of Version.to_yojson]
}
[@@deriving yojson { strict = false }]

let decode ~file content =
  {
    file;
    packages =
      List.map Pick.of_pair (Json.decode ~file written_of_yojson content).packages;
  }

let of_folder root = Json.load root path decode

let shown packages =
  let in_order = List.sort Pick.by_name packages in
  Yojson.Safe.pretty_to_string
    (written_to_yojson
       {
         packages =
           List.map (fun (pick : Pick.t) -> (pick.package, pick.version)) in_order;
       })
  ^ "\n"

let save root packages = Files.save root path (shown packages)
