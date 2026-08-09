open Ppx_compare_lib.Builtin
open Base.Export

type global = { module_name : string; exported_name : string }
[@@deriving show, compare, equal, hash]

type t = Local of string | Global of global
[@@deriving show, compare, equal, hash]

let local name = Local name
let global ~module_name ~exported_name = Global { module_name; exported_name }

let of_dotted lexeme =
  Stdlib.String.rindex_opt lexeme '.'
  |> Stdlib.Option.map (fun dot ->
         global
           ~module_name:(Stdlib.String.sub lexeme 0 dot)
           ~exported_name:
             (Stdlib.String.sub lexeme (dot + 1)
                (Stdlib.String.length lexeme - dot - 1)))
  |> Stdlib.Option.value ~default:(local lexeme)

let base = function Local name -> name | Global { exported_name; _ } -> exported_name

let to_string = function
  | Local name -> name
  | Global { module_name; exported_name } -> module_name ^ "." ^ exported_name
