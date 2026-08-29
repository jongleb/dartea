let at folder path = List.fold_left Filename.concat folder path

let kind folder path =
  match Unix.lstat (at folder path) with
  | stats -> Some stats.Unix.st_kind
  | exception Unix.Unix_error _ -> None

let is_directory folder path =
  match kind folder path with Some Unix.S_DIR -> true | _ -> false

let is_file folder path =
  match kind folder path with Some Unix.S_REG -> true | _ -> false

let load folder path =
  In_channel.with_open_bin (at folder path) In_channel.input_all

let ensured folder path =
  let rec making made = function
    | [] -> ()
    | segment :: rest ->
        let next = Relative.extended made segment in
        if not (is_directory folder next) then
          Unix.mkdir (at folder next) 0o755;
        making next rest
  in
  making Relative.root path

let saved folder path content =
  ensured folder (Relative.parent path);
  Out_channel.with_open_bin (at folder path) (fun out ->
      Out_channel.output_string out content)

let here = Filename.current_dir_name ^ Filename.dir_sep

let shown folder =
  if String.starts_with ~prefix:here folder then
    String.sub folder (String.length here)
      (String.length folder - String.length here)
  else folder
let hidden entry = String.starts_with ~prefix:"." entry

let entries folder path =
  List.sort String.compare (Array.to_list (Sys.readdir (at folder path)))

let rec walked ~suffix folder path =
  entries folder path
  |> List.concat_map (fun entry ->
         if hidden entry then []
         else
           let found = Relative.extended path entry in
           if is_directory folder found then walked ~suffix folder found
           else if String.ends_with ~suffix entry then [ found ]
           else [])

let under ~suffix folder = walked ~suffix folder Relative.root
