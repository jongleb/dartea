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
