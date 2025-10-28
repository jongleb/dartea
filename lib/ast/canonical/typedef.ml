open Data

module rec Impl : sig
  type t = { parameters : t list; body : Kind.t } [@@deriving show, fields]

  val of_frontend : Frontend.Typedef.Impl.t -> t
end = struct
  type t = { parameters : t list; body : Kind.t } [@@deriving show, fields]

  let rec of_frontend (typ : Frontend.Typedef.Impl.t) =
    {
      parameters = List.map of_frontend typ.Frontend.Typedef.Impl.parameters;
      body = Kind.of_frontend typ.body;
    }
end

and Kind : sig
  type t =
    | Tkind_concrete of string Located.t
    | Tkind_var of string Data.Located.t
    | Tkind_record of Type_record.t
    | Tkind_tuple of Impl.t list
    | Tkind_function of Type_function.t
    | Tkind_unit
  [@@deriving show]

  val of_frontend : Frontend.Typedef.Kind.t -> t
end = struct
  type t =
    | Tkind_concrete of string Located.t
    | Tkind_var of string Data.Located.t
    | Tkind_record of Type_record.t
    | Tkind_tuple of Impl.t list
    | Tkind_function of Type_function.t
    | Tkind_unit
  [@@deriving show]

  let of_frontend = function
    | Frontend.Typedef.Kind.Tkind_concrete name -> Tkind_concrete name
    | Tkind_var name -> Tkind_var name
    | Tkind_record record -> Tkind_record (Type_record.of_frontend record)
    | Tkind_tuple types -> Tkind_tuple (List.map Impl.of_frontend types)
    | Tkind_function func -> Tkind_function (Type_function.of_frontend func)
    | Tkind_unit -> Tkind_unit
end

and Type_record_row : sig
  type t = { name : string Data.Located.t; body : Impl.t }
  [@@deriving show, fields]

  val of_frontend : Frontend.Typedef.Type_record_row.t -> t
end = struct
  type t = { name : string Data.Located.t; body : Impl.t }
  [@@deriving show, fields]

  let of_frontend (row : Frontend.Typedef.Type_record_row.t) =
    {
      name = row.Frontend.Typedef.Type_record_row.name;
      body = Impl.of_frontend row.body;
    }
end

and Type_record : sig
  type t = {
    values : Type_record_row.t list;
    row_type : string Data.Located.t option;
  }
  [@@deriving show, fields]

  val of_frontend : Frontend.Typedef.Type_record.t -> t
end = struct
  type t = {
    values : Type_record_row.t list;
    row_type : string Data.Located.t option;
  }
  [@@deriving show, fields]

  let of_frontend (record : Frontend.Typedef.Type_record.t) =
    {
      values =
        List.map Type_record_row.of_frontend
          record.Frontend.Typedef.Type_record.values;
      row_type = record.row_type;
    }
end

and Type_function : sig
  type t = { arguments : Impl.t list } [@@deriving show, fields]

  val of_frontend : Frontend.Typedef.Type_function.t -> t
end = struct
  type t = { arguments : Impl.t list } [@@deriving show, fields]

  let of_frontend (func : Frontend.Typedef.Type_function.t) =
    {
      arguments =
        List.map Impl.of_frontend func.Frontend.Typedef.Type_function.arguments;
    }
end
