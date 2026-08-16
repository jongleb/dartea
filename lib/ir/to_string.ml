open Ast

let atom = function
  | A_var name -> name
  | A_int value -> string_of_int value
  | A_float value -> Printf.sprintf "%g" value
  | A_string value -> Printf.sprintf "%S" value
  | A_char value -> Printf.sprintf "'%s'" value
  | A_unit -> "()"
  | A_nil -> "[]"
  | A_constant name -> Data.Name.to_string name
  | A_global name -> "@" ^ Data.Name.to_string name

let atom_list items = String.concat ", " (List.map atom items)
let names items = String.concat ", " items
let indent lines = List.map (fun line -> "  " ^ line) lines

let field_list fields =
  String.concat ", "
    (List.map (fun (label, value) -> label ^ " = " ^ atom value) fields)

let step = function
  | Occurrence.Payload index -> Printf.sprintf "payload %d of" index
  | Occurrence.Index index -> Printf.sprintf "index %d of" index
  | Occurrence.Field label -> Printf.sprintf "field %s of" label
  | Occurrence.Hd -> "head of"
  | Occurrence.Tl -> "tail of"

let test = function
  | Decision_tree.Test_ctor name -> "ctor " ^ Data.Name.to_string name
  | Decision_tree.Test_tag name -> "tag " ^ Data.Name.to_string name
  | Decision_tree.Test_int value -> "int " ^ string_of_int value
  | Decision_tree.Test_str value -> Printf.sprintf "string %S" value
  | Decision_tree.Test_chr value -> Printf.sprintf "char '%s'" value
  | Decision_tree.Test_nil -> "nil"
  | Decision_tree.Test_cons -> "cons"

let rec term_lines = function
  | T_return value -> [ "return " ^ atom value ]
  | T_let { name; bind; body } ->
      let header, nested = bind_lines bind in
      ((("let " ^ name ^ " = " ^ header) :: nested) @ term_lines body)
  | T_if { condition; consequent; alternative } ->
      (("if " ^ atom condition) :: indent (term_lines consequent))
      @ ("else" :: indent (term_lines alternative))
  | T_switch { subject; branches; default } ->
      let branch_lines (choice, child) =
        (test choice ^ ":") :: indent (term_lines child)
      in
      let default_lines =
        match default with
        | None -> []
        | Some child -> "default:" :: indent (term_lines child)
      in
      ("switch " ^ atom subject)
      :: indent (List.concat_map branch_lines branches @ default_lines)
  | T_join { label; parameters; definition; body } ->
      ((Printf.sprintf "join %s(%s)" label (names parameters)
       :: indent (term_lines definition))
      @ term_lines body)
  | T_jump { label; arguments } ->
      [ Printf.sprintf "jump %s(%s)" label (atom_list arguments) ]
  | T_fail { message } -> [ Printf.sprintf "fail %S" message ]

and bind_lines = function
  | B_closure { parameters; captures; body } ->
      ( Printf.sprintf "closure (%s) capturing (%s)" (names parameters)
          (names captures),
        indent (term_lines body) )
  | B_atom value -> (atom value, [])
  | B_construct { name; arguments } ->
      ( Printf.sprintf "construct %s(%s)" (Data.Name.to_string name)
          (atom_list arguments),
        [] )
  | B_cons { head; tail } ->
      (Printf.sprintf "cons %s %s" (atom head) (atom tail), [])
  | B_tuple { items } -> (Printf.sprintf "tuple (%s)" (atom_list items), [])
  | B_record { fields } -> (Printf.sprintf "record {%s}" (field_list fields), [])
  | B_record_update { base; fields } ->
      (Printf.sprintf "update %s {%s}" (atom base) (field_list fields), [])
  | B_access { subject; step = position } ->
      (Printf.sprintf "%s %s" (step position) (atom subject), [])
  | B_call { callee; arguments } ->
      ( Printf.sprintf "call %s(%s)" (Data.Name.to_string callee)
          (atom_list arguments),
        [] )
  | B_call_closure { callee; arguments } ->
      ( Printf.sprintf "call_closure %s(%s)" (atom callee) (atom_list arguments),
        [] )
  | B_partial { callee; arguments; missing } ->
      ( Printf.sprintf "partial %s(%s) missing %d"
          (Data.Name.to_string callee) (atom_list arguments) missing,
        [] )
  | B_primitive { operator; arguments } ->
      ( Printf.sprintf "primitive %s(%s)" (Data.Operator.lexeme operator)
          (atom_list arguments),
        [] )
  | B_kernel { kernel; arguments } ->
      ( Printf.sprintf "kernel %s(%s)" (Data.Kernel.show kernel)
          (atom_list arguments),
        [] )

let declaration_lines (declaration : declaration) =
  Printf.sprintf "%s(%s):" declaration.name (names declaration.parameters)
  :: indent (term_lines declaration.body)

let program declarations =
  String.concat "\n"
    (List.concat_map
       (fun declaration -> declaration_lines declaration @ [ "" ])
       declarations)
