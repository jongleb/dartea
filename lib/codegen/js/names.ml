module J = Ast

let reserved =
  [
    "abstract"; "arguments"; "await"; "boolean"; "break"; "byte"; "case";
    "catch"; "char"; "class"; "const"; "continue"; "debugger"; "default";
    "delete"; "do"; "double"; "else"; "enum"; "eval"; "export"; "extends";
    "false"; "final"; "finally"; "float"; "for"; "function"; "goto"; "if";
    "implements"; "import"; "in"; "instanceof"; "int"; "interface"; "let";
    "long"; "native"; "new"; "null"; "package"; "private"; "protected";
    "public"; "return"; "short"; "static"; "super"; "switch"; "synchronized";
    "this"; "throw"; "throws"; "transient"; "true"; "try"; "typeof"; "var";
    "void"; "volatile"; "while"; "with"; "yield"; "Array"; "Object"; "String";
    "Number"; "Boolean"; "Math"; "JSON"; "Date"; "RegExp"; "Map"; "Set";
    "Promise"; "Symbol"; "Error"; "console"; "globalThis"; "undefined"; "NaN";
    "Infinity"; "parseInt"; "parseFloat"; "isNaN"; "isFinite";
  ]

let is_reserved name = List.mem name reserved

let starts_an_identifier = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' | '$' -> true
  | _ -> false

let continues_an_identifier = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '$' -> true
  | _ -> false

let is_valid_js_ident s =
  String.length s > 0
  && starts_an_identifier s.[0]
  && String.for_all continues_an_identifier s

let op_char_token = function
  | '+' -> "$plus" | '-' -> "$minus" | '*' -> "$star" | '/' -> "$slash"
  | '%' -> "$percent" | '=' -> "$eq" | '<' -> "$lt" | '>' -> "$gt"
  | '&' -> "$amp" | '|' -> "$pipe" | '!' -> "$bang" | '^' -> "$caret"
  | ':' -> "$colon" | '.' -> "$dot" | '~' -> "$tilde" | '?' -> "$question"
  | '@' -> "$at" | '#' -> "$hash"
  | c -> Printf.sprintf "$u%d" (Char.code c)

let sanitize (name : string) : string =
  if is_reserved name then "$$" ^ name
  else if is_valid_js_ident name then name
  else
    "$"
    ^ String.concat ""
        (List.map op_char_token
           (List.init (String.length name) (String.get name)))

let runtime_module = Runtime.module_name

let module_ident module_name =
  sanitize (String.concat "$" (String.split_on_char '.' module_name))

let of_name (name : Data.Name.t) =
  match name with
  | Data.Name.Local local -> sanitize local
  | Data.Name.Global { module_name; exported_name } ->
      module_ident module_name ^ "." ^ sanitize exported_name

let type_ident (name : Data.Name.t) =
  match name with
  | Data.Name.Local base -> base
  | Data.Name.Global { module_name; exported_name } ->
      module_ident module_name ^ "$" ^ exported_name

let runtime_reference helper =
  J.member (J.Identifier runtime_module) helper

let curry_reference = runtime_reference Runtime.curry
let append_reference = runtime_reference Runtime.append
let equal_reference = runtime_reference Runtime.equal
let compare_reference = runtime_reference Runtime.compare

let expression_of (name : Data.Name.t) : J.expr =
  match name with
  | Data.Name.Local local -> J.Identifier (sanitize local)
  | Data.Name.Global { module_name; exported_name } ->
      J.member (J.Identifier (module_ident module_name)) (sanitize exported_name)

let located loc = sanitize (Data.Located.unwrap loc)

type t = {
  counts : (string, int) Hashtbl.t;
  siblings : (Data.Name.t, (Data.Name.t * int) list) Hashtbl.t;
  arities : (Data.Name.t, int) Hashtbl.t;
  mutable temps : int;
}

let create () =
  {
    counts = Hashtbl.create 64;
    siblings = Hashtbl.create 64;
    arities = Hashtbl.create 64;
    temps = 0;
  }

let reserve names base =
  if not (Hashtbl.mem names.counts base) then Hashtbl.replace names.counts base 1

let fresh names base =
  match Hashtbl.find_opt names.counts base with
  | None ->
      Hashtbl.replace names.counts base 1;
      base
  | Some n ->
      Hashtbl.replace names.counts base (n + 1);
      base ^ "$" ^ string_of_int n

let temp names =
  names.temps <- names.temps + 1;
  "$s" ^ string_of_int names.temps

let siblings_of names name = Hashtbl.find_opt names.siblings name
let arity_of names name = Hashtbl.find_opt names.arities name
let note_arity names name arity = Hashtbl.replace names.arities name arity
let note_siblings names name sibs = Hashtbl.replace names.siblings name sibs

let is_tag_omitted names name =
  match siblings_of names name with
  | Some siblings -> begin
      match List.filter (fun (_, arity) -> arity >= 1) siblings with
      | [ (only, _) ] -> Data.Name.equal only name
      | _ -> false
    end
  | None -> false
