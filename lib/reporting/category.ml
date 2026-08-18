type maybe_name =
  | No_name
  | Func_name of Data.Name.t
  | Ctor_name of Data.Name.t
  | Op_name of Data.Name.t
[@@deriving show]

type t =
  | List
  | Number
  | Float
  | String
  | Char
  | If
  | Case
  | Call_result of maybe_name
  | Lambda
  | Accessor of string
  | Access of string
  | Record
  | Tuple
  | Unit
  | Local of Data.Name.t
  | Foreign of Data.Name.t
[@@deriving show]

type pattern =
  | P_record
  | P_unit
  | P_tuple
  | P_list
  | P_ctor of Data.Name.t
  | P_int
  | P_str
  | P_chr
  | P_bool
[@@deriving show]
