type t = Exactly of Version.t | Upward of Version.t

let upward = "^"

let show = function
  | Exactly version -> Version.show version
  | Upward version -> upward ^ Version.show version

let after_caret written =
  String.sub written 1 (String.length written - 1)

let of_string written =
  if String.starts_with ~prefix:upward written then
    Option.map
      (fun version -> Upward version)
      (Version.of_string (after_caret written))
  else Option.map (fun version -> Exactly version) (Version.of_string written)
