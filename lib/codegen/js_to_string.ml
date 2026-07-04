(* Convert JavaScript AST to string *)

module J = Js_ast

let indent_level = 2

let rec repeat str n = if n <= 0 then "" else str ^ repeat str (n - 1)

let indent n = repeat " " (n * indent_level)

(* Convert literal to string - modern JS *)
let literal_to_string = function
  | J.Int n -> string_of_int n
  | J.Float f -> string_of_float f
  | J.String s -> "\"" ^ String.escaped s ^ "\""
  | J.Bool true -> "true"
  | J.Bool false -> "false"
  | J.Null -> "null"

(* Convert binary operator to string *)
let binop_to_string = function
  | J.Plus -> "+"
  | J.Minus -> "-"
  | J.Multiply -> "*"
  | J.Divide -> "/"
  | J.Modulo -> "%"
  | J.Equal -> "=="
  | J.NotEqual -> "!="
  | J.StrictEqual -> "==="
  | J.StrictNotEqual -> "!=="
  | J.LessThan -> "<"
  | J.LessThanOrEqual -> "<="
  | J.GreaterThan -> ">"
  | J.GreaterThanOrEqual -> ">="
  | J.And -> "&&"
  | J.Or -> "||"

(* Convert unary operator to string *)
let unop_to_string = function
  | J.Not -> "!"
  | J.Negative -> "-"
  | J.Typeof -> "typeof "

(* Convert expression to string *)
let rec expr_to_string ?(parens = false) (e : J.expr) : string =
  let wrap_if_needed s = if parens then "(" ^ s ^ ")" else s in
  match e with
  | J.Literal lit -> literal_to_string lit
  | J.Identifier id -> id
  | J.Binary { left; op; right } ->
      let s =
        expr_to_string ~parens:true left ^ " "
        ^ binop_to_string op ^ " "
        ^ expr_to_string ~parens:true right
      in
      wrap_if_needed s
  | J.Unary { op; arg } ->
      unop_to_string op ^ expr_to_string ~parens:true arg
  | J.Call { callee; args } ->
      expr_to_string callee ^ "("
      ^ String.concat ", " (List.map (expr_to_string ~parens:false) args)
      ^ ")"
  | J.Function { params; body } ->
      "function(" ^ String.concat ", " params ^ ") {\n"
      ^ stmts_to_string ~indent_lvl:1 body
      ^ "}"
  | J.Arrow { params; body } -> (
      let params_str =
        match params with
        | [ single ] -> single
        | _ -> "(" ^ String.concat ", " params ^ ")"
      in
      let body_str =
        match body with
        | J.ArrowExpr e -> expr_to_string e
        | J.ArrowBlock stmts ->
            "{\n" ^ stmts_to_string ~indent_lvl:1 stmts ^ "}"
      in
      wrap_if_needed (params_str ^ " => " ^ body_str))
  | J.Member { object_; property; computed } -> (
      let obj_str = expr_to_string ~parens:true object_ in
      if computed then obj_str ^ "[" ^ expr_to_string property ^ "]"
      else
        match property with
        | J.Literal (J.String s) -> obj_str ^ "." ^ s
        | J.Identifier s -> obj_str ^ "." ^ s
        | _ -> obj_str ^ "[" ^ expr_to_string property ^ "]")
  | J.Conditional { test; consequent; alternate } ->
      let s =
        expr_to_string ~parens:true test ^ " ? "
        ^ expr_to_string ~parens:false consequent
        ^ " : "
        ^ expr_to_string ~parens:false alternate
      in
      wrap_if_needed s
  | J.Object pairs ->
      if List.length pairs = 0 then "{}"
      else
        "{ "
        ^ String.concat ", "
            (List.map
               (fun (key, value) -> key ^ ": " ^ expr_to_string value)
               pairs)
        ^ " }"
  | J.Array elements ->
      "["
      ^ String.concat ", " (List.map (expr_to_string ~parens:false) elements)
      ^ "]"
  | J.Assignment { left; right } ->
      expr_to_string left ^ " = " ^ expr_to_string right

(* Convert statement to string *)
and stmt_to_string ?(indent_lvl = 0) (s : J.stmt) : string =
  let ind = indent indent_lvl in
  match s with
  | J.ExprStmt e -> ind ^ expr_to_string e ^ ";"
  | J.Return None -> ind ^ "return;"
  | J.Return (Some e) -> ind ^ "return " ^ expr_to_string e ^ ";"
  | J.VarDecl { name; init = None } -> ind ^ "let " ^ name ^ ";"
  | J.VarDecl { name; init = Some e } ->
      ind ^ "let " ^ name ^ " = " ^ expr_to_string e ^ ";"
  | J.ConstDecl { name; init } ->
      ind ^ "const " ^ name ^ " = " ^ expr_to_string init ^ ";"
  | J.FunctionDecl { name; params; body } ->
      ind ^ "function " ^ name ^ "(" ^ String.concat ", " params ^ ") {\n"
      ^ stmts_to_string ~indent_lvl:(indent_lvl + 1) body
      ^ ind ^ "}"
  | J.If { test; consequent; alternate = None } ->
      ind ^ "if (" ^ expr_to_string test ^ ") {\n"
      ^ stmts_to_string ~indent_lvl:(indent_lvl + 1) consequent
      ^ ind ^ "}"
  | J.If { test; consequent; alternate = Some alt } ->
      ind ^ "if (" ^ expr_to_string test ^ ") {\n"
      ^ stmts_to_string ~indent_lvl:(indent_lvl + 1) consequent
      ^ ind ^ "} else {\n"
      ^ stmts_to_string ~indent_lvl:(indent_lvl + 1) alt
      ^ ind ^ "}"
  | J.Block stmts ->
      ind ^ "{\n" ^ stmts_to_string ~indent_lvl:(indent_lvl + 1) stmts ^ ind ^ "}"
  | J.Switch { discriminant; cases } ->
      ind ^ "switch (" ^ expr_to_string discriminant ^ ") {\n"
      ^ String.concat "\n"
          (List.map (case_to_string ~indent_lvl:(indent_lvl + 1)) cases)
      ^ "\n" ^ ind ^ "}"

(* Convert case to string *)
and case_to_string ?(indent_lvl = 0) (c : J.case) : string =
  let ind = indent indent_lvl in
  let case_label =
    match c.test with
    | None -> ind ^ "default:"
    | Some test -> ind ^ "case " ^ expr_to_string test ^ ":"
  in
  let consequent_strs = List.map (stmt_to_string ~indent_lvl:(indent_lvl + 1)) c.consequent in
  case_label ^ "\n" ^ String.concat "\n" consequent_strs

(* Convert list of statements to string *)
and stmts_to_string ?(indent_lvl = 0) (stmts : J.stmt list) : string =
  String.concat "\n" (List.map (stmt_to_string ~indent_lvl) stmts) ^ "\n"

(* Convert program to string *)
let program_to_string (prog : J.program) : string =
  stmts_to_string ~indent_lvl:0 prog
