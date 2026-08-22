let here = "."
let named prefix = not (String.equal prefix "" || String.equal prefix here)
let joined prefix entry = if named prefix then prefix ^ "/" ^ entry else entry
let at folder prefix = if named prefix then Eio.Path.(folder / prefix) else folder
let hidden entry = String.starts_with ~prefix:"." entry
let displayed ~default path = Option.value (Eio.Path.native path) ~default

let is_directory path =
  match Eio.Path.kind ~follow:false path with `Directory -> true | _ -> false

let is_file path =
  match Eio.Path.kind ~follow:false path with
  | `Regular_file -> true
  | _ -> false
