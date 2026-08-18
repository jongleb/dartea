type direction = Have | Need [@@deriving show]

type t =
  | Int_float
  | String_from_int
  | String_from_float
  | String_to_int
  | String_to_float
  | Anything_to_bool
  | Anything_from_maybe
  | Arity_mismatch of { found : int; expected : int }
  | Bad_flex_super of {
      direction : direction;
      required : Data.Constraint.t;
      found : Typed.Type.t;
    }
  | Fields_missing of string list
  | Field_typo of { typo : string; possibilities : string list }
[@@deriving show]
