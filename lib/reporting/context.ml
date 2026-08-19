type t =
  | List_entry of int
  | Negate
  | Op_left of Data.Name.t
  | Op_right of Data.Name.t
  | If_condition
  | If_branch of int
  | Case_branch of int
  | Call_arity of { callee : Category.maybe_name; given : int }
  | Call_arg of { callee : Category.maybe_name; index : int }
  | Record_access of { field : string }
  | Record_update_value of string
[@@deriving show]

type pattern =
  | P_case_match of int
  | P_ctor_arg of { name : Data.Name.t; index : int }
  | P_list_entry of int
  | P_tail
[@@deriving show]
