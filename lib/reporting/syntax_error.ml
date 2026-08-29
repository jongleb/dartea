type unterminated =
  | Character [@rename "character literal"]
  | Text [@rename "string"]
  | Block_text [@rename "multi-line string"]
  | Block_comment [@rename "block comment"]
[@@deriving show, to_string]

type t =
  | Unexpected_input of { found : string }
  | Unknown_character of { found : string }
  | Unterminated of { what : unterminated }
  | Empty_character
  | Crowded_character
  | Unknown_escape of { found : string }
  | Too_many_tuple_parts of { given : int }
  | Module_name_mismatch of { expected : string }
[@@deriving show]

let describe_open = string_of_unterminated
