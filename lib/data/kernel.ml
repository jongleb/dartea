type nullary = Basics_pi | Basics_e [@@deriving show, enumerate]

type unary =
  | String_length
  | String_from_number
  | String_is_int
  | String_to_int_unsafe
  | String_is_float
  | String_to_float_unsafe
  | String_to_list
  | String_from_list
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
  | String_split
  | String_take_left
  | String_drop_left
  | Utils_compare
  | Basics_mod_by
  | Basics_remainder_by
  | Basics_atan2
  | Basics_xor
[@@deriving show, enumerate]

type language = Nullary of nullary | Unary of unary | Binary of binary
[@@deriving show, enumerate]

type platform = { name : Name.t; arity : int } [@@deriving show]
type t = Language of language | Platform of platform [@@deriving show]

let language_arity = function Nullary _ -> 0 | Unary _ -> 1 | Binary _ -> 2

let arity = function
  | Language language -> language_arity language
  | Platform platform -> platform.arity

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
  | Unary String_to_list -> string "toList"
  | Unary String_from_list -> string "fromList"
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
  | Binary String_split -> string "split"
  | Binary String_take_left -> string "takeLeft"
  | Binary String_drop_left -> string "dropLeft"
  | Binary Utils_compare -> written_as "Utils" "compare"
  | Binary Basics_mod_by -> basics "modBy"
  | Binary Basics_remainder_by -> basics "remainderBy"
  | Binary Basics_atan2 -> basics "atan2"
  | Binary Basics_xor -> basics "xor"

let by_origin =
  Hashtbl.of_seq
    (Seq.map
       (fun kernel -> (origin kernel, kernel))
       (List.to_seq all_of_language))

type reference =
  | Not_kernel
  | Unknown of { name : Name.t; module_name : string; exported_name : string }
  | Known of t

let namespace = "Elm.Kernel."

module Port = struct
  type direction = Incoming [@rename "incoming"] | Outgoing [@rename "outgoing"]
  [@@deriving to_string]

  let module_name = "Port"
  let home = namespace ^ module_name
end

let referred_to_by (name : Name.t) : reference =
  match name with
  | Name.Local _ -> Not_kernel
  | Name.Global { module_name; _ }
    when not (String.starts_with ~prefix:namespace module_name) ->
      Not_kernel
  | Name.Global { module_name; exported_name } -> begin
      let kernel_module =
        String.sub module_name (String.length namespace)
          (String.length module_name - String.length namespace)
      in
      let inside = Name.global ~module_name:kernel_module ~exported_name in
      match Hashtbl.find_opt by_origin inside with
      | Some kernel -> Known (Language kernel)
      | None -> Unknown { name = inside; module_name; exported_name }
    end
