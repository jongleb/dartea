module Elm_file = struct
  type t = { path : string; name : string; content : string }

  let dotted path =
    Filename.remove_extension path
    |> String.split_on_char '/' |> String.concat "."

  let of_path ~path content = { path; name = dotted path; content }
end

let extension = ".elm"
let source_folder = "src"
let joined prefix entry = if prefix = "" then entry else prefix ^ "/" ^ entry
let hidden entry = String.length entry > 0 && entry.[0] = '.'
let at folder prefix = if prefix = "" then folder else Eio.Path.(folder / prefix)

let rec relative_paths folder prefix =
  Eio.Path.read_dir (at folder prefix)
  |> List.sort String.compare
  |> List.concat_map (fun entry ->
         if hidden entry then []
         else
           let path = joined prefix entry in
           match Eio.Path.kind ~follow:false (at folder path) with
           | `Directory -> relative_paths folder path
           | `Regular_file when Filename.check_suffix entry extension -> [ path ]
           | _ -> [])

let sources_in root =
  match Eio.Path.kind ~follow:false (at root source_folder) with
  | `Directory -> (at root source_folder, source_folder)
  | _ -> (root, "")

let current_folder (root_dir : _ Eio.Path.t) =
  let folder, from = sources_in root_dir in
  relative_paths folder ""
  |> List.map (fun path ->
         {
           Elm_file.path = joined from path;
           name = Elm_file.dotted path;
           content = Eio.Path.load (at folder path);
         })
