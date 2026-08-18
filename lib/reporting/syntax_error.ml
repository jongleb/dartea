type unterminated = Character | Text | Block_text | Block_comment
[@@deriving show]

type t =
  | Unexpected_input of { found : string }
  | Unknown_character of { found : string }
  | Unterminated of { what : unterminated }
  | Empty_character
  | Crowded_character
  | Unknown_escape of { found : string }
  | Too_many_tuple_parts of { given : int }
[@@deriving show]

let what_is_unterminated = function
  | Character -> "character literal"
  | Text -> "string"
  | Block_text -> "multi-line string"
  | Block_comment -> "block comment"
