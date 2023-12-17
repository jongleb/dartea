open Ast.Kind.Frontend
open Data

let make_name_no_loc ~name = Located.dummy name
let make_tkind ~name = Typedef.Kind.Tkind_concrete (make_name_no_loc ~name)

let make_typedef ?(parameters = []) ~body () =
  Typedef.Impl.Fields.create ~parameters ~body

(* let make_type_record_row =    *)

(* let dummify_top_declaration = function *)

let dummify_thing Data.Located.{ thing; _ } = Data.Located.dummy thing

module Top_declaration_util = struct
  open Declaration

  let dummify_body_part { name; expr } = { name = dummify_thing name; expr }

  let dummify_type_part { name; type_alias } =
    { name = dummify_thing name; type_alias }

  let dummify { type_part_data; body_part } = { type_part_data; body_part }
end

module Typedef_util = struct
  open Typedef

  module rec Type_record_row_util : sig
    val dummify : Type_record_row.t -> Type_record_row.t
  end = struct
    open Type_record_row

    let dummify { name : string Data.Located.t; body : Impl.t } =
      { name = dummify_thing name; body = Impl_util.dummify body }
  end

  and Type_record_util : sig
    val dummify : Type_record.t -> Type_record.t
  end = struct
    open Type_record

    let dummify { values; row_type } =
      {
        values = List.map Type_record_row_util.dummify values;
        row_type = Option.map dummify_thing row_type;
      }
  end

  and Type_function_util : sig
    val dummify : Type_function.t -> Type_function.t
  end = struct
    open Type_function

    let dummify { arguments } =
      { arguments = List.map Impl_util.dummify arguments }
  end

  and Kind_util : sig
    val dummify : Kind.t -> Kind.t
  end = struct
    open Kind

    let dummify = function
      | Kind.Tkind_concrete name -> Kind.Tkind_concrete (dummify_thing name)
      | Tkind_var name -> Kind.Tkind_concrete (dummify_thing name)
      | Tkind_record record -> Tkind_record (Type_record_util.dummify record)
      | Tkind_tuple list -> Tkind_tuple (List.map Impl_util.dummify list)
      | Tkind_function func -> Tkind_function (Type_function_util.dummify func)
      | Tkind_unit -> Tkind_unit
  end

  and Impl_util : sig
    val dummify : Impl.t -> Impl.t
  end = struct
    open Impl

    let rec dummify { parameters; body } =
      {
        parameters = List.map dummify parameters;
        body = Kind_util.dummify body;
      }
  end
end

module Typealias_util = struct
  open Typealias

  let dummify { typedef; params; name } =
    {
      typedef = Typedef_util.Impl_util.dummify typedef;
      params = List.map dummify_thing params;
      name = dummify_thing name;
    }
end

module Exposing_util = struct
  open Exposing

  let dummify_privacy = function
    | Public _ -> Public Data.Located.dummy_pair
    | Private -> Private

  let dummify_upper { name; privacy } =
    { name = dummify_thing name; privacy = dummify_privacy privacy }

  let dummify_exposed = function
    | Lower str -> Lower (dummify_thing str)
    | Upper u -> Upper (dummify_upper u)

  let dummify = function
    | Open -> Open
    | Explicit lst -> Explicit (List.map dummify_exposed lst)
end

module Import_thing_util = struct
  open Import_thing

  let dummify { name; alias; exposing } =
    {
      name = dummify_thing name;
      alias;
      exposing = Exposing_util.dummify exposing;
    }
end

let dummify_all_locs = function
  | Impl.Type_alias alias -> Impl.Type_alias (Typealias_util.dummify alias)
  | Import thing -> Impl.Import (Import_thing_util.dummify thing)
  | _ -> assert false
