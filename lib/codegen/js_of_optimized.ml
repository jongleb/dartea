module J = Js_ast
module O = Optimized

(* ========== Statement-based compilation ========== *)

(* Collect let bindings into a list of statements + final expression *)
let rec collect_lets (e : O.Expr.t) : (string * O.Expr.t) list * O.Expr.t =
  match e.expr with
  | O.Expr.Expr_let { binding; body } ->
      let var_name = Data.Located.unwrap binding.bind_body.name in
      let var_value = binding.bind_body.body in
      let rest_bindings, final_expr = collect_lets body in
      ((var_name, var_value) :: rest_bindings, final_expr)
  | _ -> ([], e)

(* ========== Binary Operations ========== *)

let binop_of_string (name : string) : J.binop option =
  match name with
  | "+" -> Some J.Plus
  | "-" -> Some J.Minus
  | "*" -> Some J.Multiply
  | "/" -> Some J.Divide
  | "%" -> Some J.Modulo
  | "==" -> Some J.StrictEqual
  | "!=" -> Some J.StrictNotEqual
  | "<" -> Some J.LessThan
  | "<=" -> Some J.LessThanOrEqual
  | ">" -> Some J.GreaterThan
  | ">=" -> Some J.GreaterThanOrEqual
  | "&&" -> Some J.And
  | "||" -> Some J.Or
  | _ -> None

(* ========== Constructor Helpers ========== *)

let is_bool_constructor name = name = "True" || name = "False"
let is_unit_constructor name = name = "Unit" || name = "()"
let bool_literal name = J.Literal (J.Bool (name = "True"))

let constructor_to_object name js_arguments =
  if is_bool_constructor name then bool_literal name
  else if is_unit_constructor name then J.Literal J.Null
  else if List.length js_arguments = 0 then
    J.Object [ ("_tag", J.Literal (J.String name)) ]
  else
    J.Object
      [ ("_tag", J.Literal (J.String name)); ("_args", J.Array js_arguments) ]

(* ========== Record Construction ========== *)

let rec is_record_construction (expr_node : O.Expr.t) =
  match expr_node.expr with
  | O.Expr.Expr_record_extend _ -> true
  | O.Expr.Expr_apply { fn; _ } -> is_record_construction fn
  | _ -> false

let rec extract_record_fields (expr_node : O.Expr.t) : (string * O.Expr.t) list
    =
  match expr_node.expr with
  | O.Expr.Expr_apply { fn; arg } -> (
      match fn.expr with
      | O.Expr.Expr_record_extend field -> [ (field, arg) ]
      | O.Expr.Expr_apply { fn = inner_fn; arg = inner_arg } -> (
          match inner_fn.expr with
          | O.Expr.Expr_record_extend field ->
              (field, inner_arg) :: extract_record_fields arg
          | _ -> [])
      | _ -> [])
  | O.Expr.Expr_record_empty -> []
  | _ -> []

(* ========== Function Application ========== *)

let rec collect_args acc (fn : O.Expr.t) =
  match fn.expr with
  | O.Expr.Expr_apply { fn = inner_fn; arg = inner_arg } ->
      collect_args (inner_arg :: acc) inner_fn
  | _ -> (fn, acc)

(* ========== List Encoding ========== *)

let rec list_to_cons_cells expr_converter = function
  | [] -> J.Literal (J.Int 0)
  | hd :: tl ->
      J.Object
        [
          ("hd", expr_converter hd); ("tl", list_to_cons_cells expr_converter tl);
        ]

(* ========== Pattern Matching Utilities ========== *)

let needs_temp_var = function
  | J.Identifier _ | J.Literal _ -> false
  | _ -> true

let make_temp_var e = "_match$" ^ string_of_int (Hashtbl.hash e)

let wrap_with_bindings (bindings : (string * J.expr) list) (result : J.expr) :
    J.expr =
  match bindings with
  | [] -> result
  | _ ->
      let params = List.map fst bindings in
      let args = List.map snd bindings in
      J.Call { callee = J.Arrow { params; body = J.ArrowExpr result }; args }

let member object_ property =
  J.Member { object_; property = J.Identifier property; computed = false }

let indexed_member object_ index =
  J.Member { object_; property = J.Literal (J.Int index); computed = true }

(* ========== Pattern Compilation ========== *)

let rec pattern_to_test_expr_js (scrutinee : J.expr) (pattern : O.Pattern.t)
    (bindings : (string * J.expr) list ref) : J.expr =
  match pattern.pattern with
  | O.Pattern.P_T_anything -> J.Literal (J.Bool true)
  | O.Pattern.P_T_var name ->
      bindings := (name, scrutinee) :: !bindings;
      J.Literal (J.Bool true)
  | O.Pattern.P_T_int n ->
      J.Binary
        { left = scrutinee; op = J.StrictEqual; right = J.Literal (J.Int n) }
  | O.Pattern.P_T_str s ->
      J.Binary
        { left = scrutinee; op = J.StrictEqual; right = J.Literal (J.String s) }
  | O.Pattern.P_T_chr c ->
      J.Binary
        { left = scrutinee; op = J.StrictEqual; right = J.Literal (J.String c) }
  | O.Pattern.P_T_ctor (name, args) ->
      compile_constructor_pattern scrutinee name args bindings
  | O.Pattern.P_T_list patterns ->
      compile_list_pattern scrutinee patterns bindings
  | O.Pattern.P_T_unit -> J.Literal (J.Bool true)
  | O.Pattern.P_T_tuple patterns ->
      compile_tuple_pattern scrutinee patterns bindings
  | O.Pattern.P_T_cons (hd_pat, tl_pat) ->
      compile_cons_pattern scrutinee hd_pat tl_pat bindings
  | O.Pattern.P_T_record _ -> J.Literal (J.Bool true)

and compile_constructor_pattern scrutinee name args bindings =
  if is_bool_constructor name then
    J.Binary { left = scrutinee; op = J.StrictEqual; right = bool_literal name }
  else
    let tag_check =
      J.Binary
        {
          left = member scrutinee "_tag";
          op = J.StrictEqual;
          right = J.Literal (J.String name);
        }
    in
    if args = [] then tag_check
    else (
      extract_constructor_args scrutinee args bindings;
      tag_check)

and extract_constructor_args scrutinee args bindings =
  List.iteri
    (fun i arg_pattern ->
      match arg_pattern.O.Pattern.pattern with
      | O.Pattern.P_T_var var_name ->
          let arg_expr = indexed_member (member scrutinee "_args") i in
          bindings := (var_name, arg_expr) :: !bindings
      | _ -> ())
    args

and compile_list_pattern scrutinee patterns bindings =
  if patterns = [] then
    J.Binary
      { left = scrutinee; op = J.StrictEqual; right = J.Literal (J.Int 0) }
  else
    let rec check_list current_expr = function
      | [] ->
          J.Binary
            {
              left = member current_expr "tl";
              op = J.StrictEqual;
              right = J.Literal (J.Int 0);
            }
      | pat :: rest ->
          bind_list_element current_expr "hd" pat bindings;
          if rest = [] then
            J.Binary
              {
                left = member current_expr "tl";
                op = J.StrictEqual;
                right = J.Literal (J.Int 0);
              }
          else check_list (member current_expr "tl") rest
    in
    check_list scrutinee patterns

and bind_list_element current_expr field pat bindings =
  match pat.O.Pattern.pattern with
  | O.Pattern.P_T_var var_name ->
      bindings := (var_name, member current_expr field) :: !bindings
  | _ -> ()

and compile_tuple_pattern scrutinee patterns bindings =
  List.iteri
    (fun i pat ->
      match pat.O.Pattern.pattern with
      | O.Pattern.P_T_var var_name ->
          bindings := (var_name, indexed_member scrutinee i) :: !bindings
      | _ -> ())
    patterns;
  J.Binary
    {
      left = member scrutinee "length";
      op = J.StrictEqual;
      right = J.Literal (J.Int (List.length patterns));
    }

and compile_cons_pattern scrutinee hd_pat tl_pat bindings =
  bind_list_element scrutinee "hd" hd_pat bindings;
  bind_list_element scrutinee "tl" tl_pat bindings;
  J.Binary
    { left = scrutinee; op = J.StrictNotEqual; right = J.Literal (J.Int 0) }

(* ========== Expression Conversion ========== *)

let rec expr_of_optimized (e : O.Expr.t) : J.expr =
  match e.expr with
  | O.Expr.Expr_int n -> J.Literal (J.Int n)
  | O.Expr.Expr_float f -> J.Literal (J.Float f)
  | O.Expr.Expr_string s -> J.Literal (J.String s)
  | O.Expr.Expr_char c -> J.Literal (J.String c)
  | O.Expr.Expr_ident name -> J.Identifier name
  | O.Expr.Expr_binop { name; operands = e1, e2 } -> compile_binop name e1 e2
  | O.Expr.Expr_constr { name; arguments } ->
      constructor_to_object name (List.map expr_of_optimized arguments)
  | O.Expr.Expr_apply { fn; arg } -> compile_apply fn arg
  | O.Expr.Expr_lambda { params; body } -> compile_lambda params body
  | O.Expr.Expr_let { binding; body } -> compile_let binding body
  | O.Expr.Expr_if_then_else { if_exp; then_exp; else_exp } ->
      J.Conditional
        {
          test = expr_of_optimized if_exp;
          consequent = expr_of_optimized then_exp;
          alternate = expr_of_optimized else_exp;
        }
  | O.Expr.Expr_record rows ->
      let pairs =
        List.map
          (fun { O.Expr.name; value } -> (name, expr_of_optimized value))
          rows
      in
      J.Object pairs
  | O.Expr.Expr_access { expr; field } ->
      J.Member
        {
          object_ = expr_of_optimized expr;
          property = J.Identifier (Data.Located.unwrap field);
          computed = false;
        }
  | O.Expr.Expr_list es -> list_to_cons_cells expr_of_optimized es
  | O.Expr.Expr_pattern { expr; pattern_data_items } ->
      compile_pattern_match expr pattern_data_items
  | O.Expr.Expr_accessor field ->
      J.Arrow
        {
          params = [ "r" ];
          body =
            J.ArrowExpr (member (J.Identifier "r") (Data.Located.unwrap field));
        }
  | O.Expr.Expr_record_extend name -> J.Identifier name
  | O.Expr.Expr_record_select name -> J.Identifier name
  | O.Expr.Expr_record_empty -> J.Object []

and compile_binop name e1 e2 =
  let left = expr_of_optimized e1 in
  let right = expr_of_optimized e2 in
  match binop_of_string name with
  | Some op -> J.Binary { left; op; right }
  | None -> J.Call { callee = J.Identifier name; args = [ left; right ] }

and compile_apply fn arg =
  if is_record_construction fn then compile_record_apply fn arg
  else compile_function_apply fn arg

and compile_record_apply fn arg =
  let apply_expr =
    { O.Expr.typ = fn.O.Expr.typ; expr = O.Expr.Expr_apply { fn; arg } }
  in
  let fields = extract_record_fields apply_expr in
  if fields <> [] then
    J.Object
      (List.rev (List.map (fun (n, v) -> (n, expr_of_optimized v)) fields))
  else
    J.Call { callee = expr_of_optimized fn; args = [ expr_of_optimized arg ] }

and compile_function_apply fn arg =
  let callee_expr, args_exprs = collect_args [ arg ] fn in
  match (callee_expr.expr, args_exprs) with
  | O.Expr.Expr_ident op_name, [ arg1; arg2 ] -> (
      match binop_of_string op_name with
      | Some op ->
          J.Binary
            {
              left = expr_of_optimized arg1;
              op;
              right = expr_of_optimized arg2;
            }
      | None ->
          J.Call
            {
              callee = expr_of_optimized callee_expr;
              args = List.map expr_of_optimized args_exprs;
            })
  | _ ->
      J.Call
        {
          callee = expr_of_optimized callee_expr;
          args = List.map expr_of_optimized args_exprs;
        }

and compile_lambda params body =
  let param_names =
    List.map
      (fun (p : O.Expr.expr_lambda_param) -> Data.Located.unwrap p.name)
      params
  in
  J.Arrow { params = param_names; body = J.ArrowExpr (expr_of_optimized body) }

and compile_let binding body =
  let var_name = Data.Located.unwrap binding.bind_body.name in
  let var_value = expr_of_optimized binding.bind_body.body in
  let body_expr = expr_of_optimized body in
  J.Call
    {
      callee = J.Arrow { params = [ var_name ]; body = J.ArrowExpr body_expr };
      args = [ var_value ];
    }

and compile_pattern_match expr pattern_data_items =
  let scrutinee_expr = expr_of_optimized expr in
  let scrutinee_var, scrutinee_js =
    if needs_temp_var scrutinee_expr then
      let temp = make_temp_var expr in
      (temp, J.Identifier temp)
    else ("", scrutinee_expr)
  in

  let compile_case { O.Expr.pattern; expr } =
    let bindings = ref [] in
    let test = pattern_to_test_expr_js scrutinee_js pattern bindings in
    (test, !bindings, expr_of_optimized expr)
  in

  let rec build_conditional = function
    | [] ->
        J.Call { callee = J.Identifier "$$matchError"; args = [ scrutinee_js ] }
    | [ (test, bindings, result) ] ->
        if test = J.Literal (J.Bool true) && bindings = [] then result
        else wrap_with_bindings bindings result
    | (test, bindings, result) :: rest ->
        J.Conditional
          {
            test;
            consequent = wrap_with_bindings bindings result;
            alternate = build_conditional rest;
          }
  in

  let cases = List.map compile_case pattern_data_items in
  let conditional = build_conditional cases in

  if scrutinee_var <> "" then
    J.Call
      {
        callee =
          J.Arrow { params = [ scrutinee_var ]; body = J.ArrowExpr conditional };
        args = [ scrutinee_expr ];
      }
  else conditional

(* ========== Switch Statement Optimization ========== *)

let can_use_switch (pattern_data_items : O.Expr.expr_pattern_case list) : bool =
  List.for_all
    (fun { O.Expr.pattern; _ } ->
      match pattern.O.Pattern.pattern with
      | O.Pattern.P_T_int _ | O.Pattern.P_T_str _ | O.Pattern.P_T_chr _
      | O.Pattern.P_T_anything ->
          true
      | _ -> false)
    pattern_data_items

let pattern_to_switch (scrutinee_expr : J.expr)
    (pattern_data_items : O.Expr.expr_pattern_case list) : J.stmt =
  let compile_case { O.Expr.pattern; expr } =
    let test_opt =
      match pattern.O.Pattern.pattern with
      | O.Pattern.P_T_int n -> Some (J.Literal (J.Int n))
      | O.Pattern.P_T_str s -> Some (J.Literal (J.String s))
      | O.Pattern.P_T_chr c -> Some (J.Literal (J.String c))
      | O.Pattern.P_T_anything -> None
      | _ -> None
    in
    {
      J.test = test_opt;
      consequent = [ J.Return (Some (expr_of_optimized expr)) ];
    }
  in
  J.Switch
    {
      discriminant = scrutinee_expr;
      cases = List.map compile_case pattern_data_items;
    }

(* ========== Declaration Conversion ========== *)

let stmt_of_declaration (decl : O.Declaration.t) : J.stmt =
  let name = Data.Located.unwrap decl.name in
  match decl.params with
  | [] -> J.ConstDecl { name; init = expr_of_optimized decl.body }
  | params -> (
      let param_names =
        List.map
          (fun (p : O.Declaration.param) -> Data.Located.unwrap p.name)
          params
      in

      match decl.body.expr with
      | O.Expr.Expr_pattern { expr; pattern_data_items }
        when can_use_switch pattern_data_items
             &&
             match expr.expr with
             | O.Expr.Expr_ident id -> List.mem id param_names
             | _ -> false ->
          let scrutinee = expr_of_optimized expr in
          J.FunctionDecl
            {
              name;
              params = param_names;
              body = [ pattern_to_switch scrutinee pattern_data_items ];
            }
      | _ ->
          J.ConstDecl
            {
              name;
              init =
                J.Arrow
                  {
                    params = param_names;
                    body = J.ArrowExpr (expr_of_optimized decl.body);
                  };
            })

let program_of_declarations (decls : O.Declaration.t list) : J.program =
  List.map stmt_of_declaration decls

(* ========== Runtime Helpers ========== *)

let match_error_helper : J.stmt =
  J.ConstDecl
    {
      name = "$$matchError";
      init =
        J.Arrow
          {
            params = [ "value" ];
            body =
              J.ArrowBlock
                [
                  J.ExprStmt
                    (J.Call
                       {
                         callee = member (J.Identifier "console") "error";
                         args =
                           [
                             J.Literal
                               (J.String "Pattern match failed for value:");
                             J.Identifier "value";
                           ];
                       });
                  J.Return (Some (J.Literal J.Null));
                ];
          };
    }

let program_with_helpers (decls : O.Declaration.t list) : J.program =
  match_error_helper :: program_of_declarations decls
