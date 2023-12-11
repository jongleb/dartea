type t = { parameters : t list; body : kind } [@@deriving show]

and kind =
  | Tkind_concrete of string
  (* A.Region Name [Type] *)
  | Tkind_var of string (* Name *)
  | Tkind_record of
      type_record (* [(A.Located Name, Type)] (Maybe (A.Located Name)) *)
  | Tkind_tuple of t list (* Type Type [Type] *)
  | Tkind_function of type_function (* TLambda Type Type *)
  | Tkind_unit
[@@deriving show]

and type_record_row = { name : string; body : t } [@@deriving show]

and type_record = { values : type_record_row list; row_type : string option }
[@@deriving show]

and type_function = { arguments : t list } [@@deriving show]

let make ?(parameters = []) ~body () = { parameters; body }
let make_tkind_concrete ~name () = Tkind_concrete name
let make_tkind_var ~var () = Tkind_var var
let make_tkind_record ~record () = Tkind_concrete record
let make_tkind_tuple ~tuple () = Tkind_tuple tuple
let make_tkind_function ~function_ () = Tkind_function function_
let make_tkind_unit () = Tkind_unit
