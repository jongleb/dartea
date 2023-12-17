module rec Impl : sig
  type t = { parameters : t list; body : Kind.t } [@@deriving show, fields]
end = struct
  type t = { parameters : t list; body : Kind.t } [@@deriving show, fields]
end

and Kind : sig
  type t =
    | Tkind_concrete of string Data.Located.t
    | Tkind_var of string Data.Located.t
    | Tkind_record of Type_record.t
    | Tkind_tuple of Impl.t list
    | Tkind_function of Type_function.t
    | Tkind_unit
  [@@deriving show]
end = struct
  type t =
    | Tkind_concrete of string Data.Located.t
    (* A.Region Name [Type] *)
    | Tkind_var of string Data.Located.t (* Name *)
    | Tkind_record of Type_record.t
    (* [(A.Located Name, Type)] (Maybe (A.Located Name)) *)
    | Tkind_tuple of Impl.t list (* Type Type [Type] *)
    | Tkind_function of Type_function.t (* TLambda Type Type *)
    | Tkind_unit
  [@@deriving show]
end

and Type_record_row : sig
  type t = { name : string Data.Located.t; body : Impl.t }
  [@@deriving show, fields]
end = struct
  type t = { name : string Data.Located.t; body : Impl.t }
  [@@deriving show, fields]
end

and Type_record : sig
  type t = {
    values : Type_record_row.t list;
    row_type : string Data.Located.t option;
  }
  [@@deriving show, fields]
end = struct
  type t = {
    values : Type_record_row.t list;
    row_type : string Data.Located.t option;
  }
  [@@deriving show, fields]
end

and Type_function : sig
  type t = { arguments : Impl.t list } [@@deriving show, fields]
end = struct
  type t = { arguments : Impl.t list } [@@deriving show, fields]
end
