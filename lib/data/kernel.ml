type unary =
  | String_length
  | String_from_number
  | String_is_int
  | String_to_int_unsafe
[@@deriving show]

type binary = String_append [@@deriving show]
type t = Unary of unary | Binary of binary [@@deriving show]

let arity = function Unary _ -> 1 | Binary _ -> 2

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
  let implemented kernel_module exported_name =
    match (kernel_module, exported_name) with
    | "String", "length" -> Some (Unary String_length)
    | "String", "append" -> Some (Binary String_append)
    | "String", "fromNumber" -> Some (Unary String_from_number)
    | "String", "isInt" -> Some (Unary String_is_int)
    | "String", "toIntUnsafe" -> Some (Unary String_to_int_unsafe)
    | _ -> None
  in
  match name with
  | Name.Local _ -> Not_kernel
  | Name.Global { module_name; exported_name } -> begin
      match inside_namespace module_name with
      | None -> Not_kernel
      | Some kernel_module -> begin
          match implemented kernel_module exported_name with
          | Some kernel -> Known kernel
          | None -> Unknown { module_name; exported_name }
        end
    end
