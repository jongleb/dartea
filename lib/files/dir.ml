let at folder path =
  List.fold_left (fun folder segment -> Eio.Path.(folder / segment)) folder path

let kind folder path = Eio.Path.kind ~follow:false (at folder path)

let is_directory folder path =
  match kind folder path with `Directory -> true | _ -> false

let is_file folder path =
  match kind folder path with `Regular_file -> true | _ -> false

let load folder path = Eio.Path.load (at folder path)

let ensured folder path =
  let rec making made = function
    | [] -> ()
    | segment :: rest ->
        let next = Relative.extended made segment in
        if not (is_directory folder next) then
          Eio.Path.mkdir ~perm:0o755 (at folder next);
        making next rest
  in
  making Relative.root path

let saved folder path content =
  ensured folder (Relative.parent path);
  Eio.Path.save ~create:(`Or_truncate 0o644) (at folder path) content

let shown folder =
  Option.value (Eio.Path.native folder) ~default:Filename.current_dir_name

let hidden entry = String.starts_with ~prefix:"." entry

let rec walked ~suffix folder path =
  Eio.Path.read_dir (at folder path)
  |> List.sort String.compare
  |> List.concat_map (fun entry ->
         if hidden entry then []
         else
           let found = Relative.extended path entry in
           if is_directory folder found then walked ~suffix folder found
           else if String.ends_with ~suffix entry then [ found ]
           else [])

let under ~suffix folder = walked ~suffix folder Relative.root
