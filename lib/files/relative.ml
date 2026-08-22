type t = string list

let separator = '/'
let root = []

let of_string written =
  String.split_on_char separator written
  |> List.filter (fun segment ->
         not
           (String.equal segment ""
           || String.equal segment Filename.current_dir_name))

let inside parent child = parent @ child

let parent path =
  match List.rev path with [] -> [] | _ :: earlier -> List.rev earlier
let extended path segment = path @ [ segment ]
let shown path = String.concat (String.make 1 separator) path
