type t = { major : int; minor : int; patch : int }

let parts version = [ version.major; version.minor; version.patch ]
let show version = String.concat "." (List.map string_of_int (parts version))
let compare one other = List.compare Int.compare (parts one) (parts other)
let next_major version = { major = version.major + 1; minor = 0; patch = 0 }
let next_patch version = { version with patch = version.patch + 1 }

let digits written =
  String.length written > 0
  && String.for_all (function '0' .. '9' -> true | _ -> false) written

let number written =
  if digits written then int_of_string_opt written else None

let of_string written =
  match List.map number (String.split_on_char '.' written) with
  | [ Some major; Some minor; Some patch ] -> Some { major; minor; patch }
  | _ -> None
