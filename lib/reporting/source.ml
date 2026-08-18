type t = { file : string; lines : string array }

let of_string ~file content =
  { file; lines = Array.of_list (String.split_on_char '\n' content) }

let line_at source number =
  if number >= 1 && number <= Array.length source.lines then
    Some source.lines.(number - 1)
  else None

let file source = source.file
