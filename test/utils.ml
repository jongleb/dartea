(* open Dartea *)
open Ast
open Data

let make_type_alias_no_loc ~name = Typealias.make ~name:(Located.dummy name)

let make_type_alias_no_loc_top ~name ~typedef ?params () =
  Impl.Type_alias (make_type_alias_no_loc ~name ~typedef ?params ())

(* let make_type_def_no_loc   *)
(* let make_type_def_no_loc  *)
