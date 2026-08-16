type nullary = Basics_pi | Basics_e [@@deriving show, enumerate]

type unary =
  | String_length
  | String_from_number
  | String_is_int
  | String_to_int_unsafe
  | String_is_float
  | String_to_float_unsafe
  | Basics_to_float
  | Basics_round
  | Basics_floor
  | Basics_ceiling
  | Basics_truncate
  | Basics_is_nan
  | Basics_is_infinite
  | Basics_sqrt
  | Basics_log
  | Basics_cos
  | Basics_sin
  | Basics_tan
  | Basics_acos
  | Basics_asin
  | Basics_atan
  | Basics_not
  | Char_to_code
  | Char_from_code
  | Char_to_upper
  | Char_to_lower
[@@deriving show, enumerate]

type binary =
  | String_append
  | Utils_compare
  | Basics_mod_by
  | Basics_remainder_by
  | Basics_atan2
  | Basics_xor
[@@deriving show, enumerate]

type t = Nullary of nullary | Unary of unary | Binary of binary
[@@deriving show, enumerate]

let arity = function Nullary _ -> 0 | Unary _ -> 1 | Binary _ -> 2

let written_as module_name exported_name =
  Name.global ~module_name ~exported_name

let basics = written_as "Basics"
let string = written_as "String"
let char = written_as "Char"

let origin = function
  | Nullary Basics_pi -> basics "pi"
  | Nullary Basics_e -> basics "e"
  | Unary String_length -> string "length"
  | Unary String_from_number -> string "fromNumber"
  | Unary String_is_int -> string "isInt"
  | Unary String_to_int_unsafe -> string "toIntUnsafe"
  | Unary String_is_float -> string "isFloat"
  | Unary String_to_float_unsafe -> string "toFloatUnsafe"
  | Unary Basics_to_float -> basics "toFloat"
  | Unary Basics_round -> basics "round"
  | Unary Basics_floor -> basics "floor"
  | Unary Basics_ceiling -> basics "ceiling"
  | Unary Basics_truncate -> basics "truncate"
  | Unary Basics_is_nan -> basics "isNaN"
  | Unary Basics_is_infinite -> basics "isInfinite"
  | Unary Basics_sqrt -> basics "sqrt"
  | Unary Basics_log -> basics "log"
  | Unary Basics_cos -> basics "cos"
  | Unary Basics_sin -> basics "sin"
  | Unary Basics_tan -> basics "tan"
  | Unary Basics_acos -> basics "acos"
  | Unary Basics_asin -> basics "asin"
  | Unary Basics_atan -> basics "atan"
  | Unary Basics_not -> basics "not"
  | Unary Char_to_code -> char "toCode"
  | Unary Char_from_code -> char "fromCode"
  | Unary Char_to_upper -> char "toUpper"
  | Unary Char_to_lower -> char "toLower"
  | Binary String_append -> string "append"
  | Binary Utils_compare -> written_as "Utils" "compare"
  | Binary Basics_mod_by -> basics "modBy"
  | Binary Basics_remainder_by -> basics "remainderBy"
  | Binary Basics_atan2 -> basics "atan2"
  | Binary Basics_xor -> basics "xor"

let by_origin = Lookup.by ~key:origin all

type reference =
  | Not_kernel
  | Unknown of { module_name : string; exported_name : string }
  | Known of t

let namespace = "Elm.Kernel."

let referred_to_by (name : Name.t) : reference =
  let inside_namespace module_name =
    let prefix = String.length namespace in
    if String.starts_with ~prefix:namespace module_name then
      Some (String.sub module_name prefix (String.length module_name - prefix))
    else None
  in
  match name with
  | Name.Local _ -> Not_kernel
  | Name.Global { module_name; exported_name } -> begin
      match inside_namespace module_name with
      | None -> Not_kernel
      | Some kernel_module -> begin
          match
            Hashtbl.find_opt by_origin
              (Name.global ~module_name:kernel_module ~exported_name)
          with
          | Some kernel -> Known kernel
          | None -> Unknown { module_name; exported_name }
        end
    end
