type sub =
  | Typed_if_branch of int
  | Typed_case_branch of int
  | Typed_body
[@@deriving show]

type t =
  | No_expectation of Typed.Type.t
  | From_context of { context : Context.t; expected : Typed.Type.t }
  | From_annotation of { name : string; sub : sub; expected : Typed.Type.t }
[@@deriving show]

type pattern =
  | Pattern_no_expectation of Typed.Type.t
  | Pattern_from_context of { context : Context.pattern; expected : Typed.Type.t }
[@@deriving show]

let expected_type = function
  | No_expectation expected -> expected
  | From_context { expected; _ } -> expected
  | From_annotation { expected; _ } -> expected

let expected_pattern_type = function
  | Pattern_no_expectation expected -> expected
  | Pattern_from_context { expected; _ } -> expected
