module J = Js_ast
module O = Optimized

let js_reserved =
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

let reserved =
  let h = Hashtbl.create 128 in
  List.iter (fun w -> Hashtbl.replace h w ()) js_reserved;
  h

let is_reserved name = Hashtbl.mem reserved name

let is_valid_js_ident s =
  String.length s > 0
  && (match s.[0] with 'A' .. 'Z' | 'a' .. 'z' | '_' | '$' -> true | _ -> false)
  && String.for_all
       (function 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '$' -> true | _ -> false)
       s

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

let jid name = J.Identifier (sanitize name)
let sname loc = sanitize (Data.Located.unwrap loc)

let temp_counter = ref 0

module SMap = Map.Make (String)

let name_counts : (string, int) Hashtbl.t = Hashtbl.create 64

let arities : (string, int) Hashtbl.t = Hashtbl.create 64

let reset_names () =
  Hashtbl.clear name_counts;
  Hashtbl.clear arities;
  temp_counter := 0

let reserve_name base =
  if not (Hashtbl.mem name_counts base) then Hashtbl.replace name_counts base 1

let fresh_js base =
  match Hashtbl.find_opt name_counts base with
  | None ->
      Hashtbl.replace name_counts base 1;
      base
  | Some n ->
      Hashtbl.replace name_counts base (n + 1);
      base ^ "$" ^ string_of_int n

let bind_one env src =
  let js = fresh_js (sanitize src) in
  (SMap.add src js env, js)

let ref_name env src =
  match SMap.find_opt src env with Some js -> js | None -> sanitize src

let jid_env env src = J.Identifier (ref_name env src)

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

let is_bool_constructor name = name = "True" || name = "False"
let is_unit_constructor name = name = "Unit" || name = "()"
let bool_literal name = J.Literal (J.Bool (name = "True"))

let constructor_to_object name js_arguments =
  if is_bool_constructor name then bool_literal name
  else if is_unit_constructor name then J.Literal J.Null
  else if js_arguments = [] then J.Literal (J.String name)
  else
    J.Object
      (("TAG", J.Literal (J.String name))
      :: List.mapi (fun i a -> ("_" ^ string_of_int i, a)) js_arguments)

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

let rec collect_args acc (fn : O.Expr.t) =
  match fn.expr with
  | O.Expr.Expr_apply { fn = inner_fn; arg = inner_arg } ->
      collect_args (inner_arg :: acc) inner_fn
  | _ -> (fn, acc)

let rec list_to_cons_cells = function
  | [] -> J.Literal (J.Int 0)
  | hd :: tl -> J.Object [ ("hd", hd); ("tl", list_to_cons_cells tl) ]

let needs_temp_var = function
  | J.Identifier _ | J.Literal _ -> false
  | _ -> true

let fresh_temp () =
  incr temp_counter;
  "$s" ^ string_of_int !temp_counter

let member object_ property =
  J.Member { object_; property = J.Identifier property; computed = false }

let indexed_member object_ index =
  J.Member { object_; property = J.Literal (J.Int index); computed = true }

let ctor_arg occ i = member occ ("_" ^ string_of_int i)
let js_eq left lit = J.Binary { left; op = J.StrictEqual; right = J.Literal lit }
let js_and a b = J.Binary { left = a; op = J.And; right = b }

let js_ne_zero occ =
  J.Binary { left = occ; op = J.StrictNotEqual; right = J.Literal (J.Int 0) }

let and_opt a b =
  match (a, b) with
  | None, x | x, None -> x
  | Some x, Some y -> Some (js_and x y)

let rec match_pattern (occ : J.expr) (pat : O.Pattern.t) :
    J.expr option * (string * J.expr) list =
  match pat.O.Pattern.pattern with
  | O.Pattern.P_T_anything | O.Pattern.P_T_unit -> (None, [])
  | O.Pattern.P_T_var name -> (None, [ (name, occ) ])
  | O.Pattern.P_T_int n -> (Some (js_eq occ (J.Int n)), [])
  | O.Pattern.P_T_str s -> (Some (js_eq occ (J.String s)), [])
  | O.Pattern.P_T_chr c -> (Some (js_eq occ (J.String c)), [])
  | O.Pattern.P_T_record fields ->
      (None, List.map (fun f -> (f, member occ f)) fields)
  | O.Pattern.P_T_tuple pats ->
      combine (List.mapi (fun i p -> (indexed_member occ i, p)) pats)
  | O.Pattern.P_T_ctor (name, args) ->
      if is_bool_constructor name then
        (Some (js_eq occ (J.Bool (name = "True"))), [])
      else if args = [] then

        (Some (js_eq occ (J.String name)), [])
      else

        let tag_test = js_eq (member occ "TAG") (J.String name) in
        let sub_test, sub_binds =
          combine (List.mapi (fun i p -> (ctor_arg occ i, p)) args)
        in
        (and_opt (Some tag_test) sub_test, sub_binds)
  | O.Pattern.P_T_cons (hd, tl) ->
      let hd_t, hd_b = match_pattern (member occ "hd") hd in
      let tl_t, tl_b = match_pattern (member occ "tl") tl in
      (and_opt (Some (js_ne_zero occ)) (and_opt hd_t tl_t), hd_b @ tl_b)
  | O.Pattern.P_T_list pats -> match_list occ pats

and combine (items : (J.expr * O.Pattern.t) list) :
    J.expr option * (string * J.expr) list =
  List.fold_left
    (fun (test_acc, binds_acc) (occ, pat) ->
      let t, b = match_pattern occ pat in
      (and_opt test_acc t, binds_acc @ b))
    (None, []) items

and match_list occ = function
  | [] -> (Some (js_eq occ (J.Int 0)), [])
  | p :: rest ->
      let hd_t, hd_b = match_pattern (member occ "hd") p in
      let rest_t, rest_b = match_list (member occ "tl") rest in
      (and_opt (Some (js_ne_zero occ)) (and_opt hd_t rest_t), hd_b @ rest_b)

let match_error occ =
  J.Call { callee = J.Identifier "$$matchError"; args = [ occ ] }

let assign_stmt r e = J.ExprStmt (J.Assignment { left = J.Identifier r; right = e })

let binop_expr name ea eb =
  match binop_of_string name with
  | Some op -> J.Binary { left = ea; op; right = eb }
  | None -> J.Call { callee = jid name; args = [ ea; eb ] }

let curry_call f args =
  J.Call { callee = J.Identifier "$$curry"; args = [ f; J.Array args ] }

let split_at n lst =
  let rec go i acc = function
    | rest when i = 0 -> (List.rev acc, rest)
    | x :: rest -> go (i - 1) (x :: acc) rest
    | [] -> (List.rev acc, [])
  in
  go n [] lst

let is_operator env name =
  (not (SMap.mem name env)) && binop_of_string name <> None

let top_arity env name =
  if SMap.mem name env then None
  else match Hashtbl.find_opt arities name with Some n when n >= 1 -> Some n | _ -> None

let closure_partial callee args missing =
  let rparams = List.init missing (fun _ -> fresh_temp ()) in
  let rargs = List.map (fun p -> J.Identifier p) rparams in
  J.Arrow
    {
      params = rparams;
      body = J.ArrowExpr (J.Call { callee; args = args @ rargs });
    }

let is_wildcard_pat (p : O.Pattern.t) =
  match p.O.Pattern.pattern with
  | O.Pattern.P_T_anything | O.Pattern.P_T_var _ -> true
  | _ -> false

let fold_emit (f : 'a -> J.stmt list * 'b) (items : 'a list) :
    J.stmt list * 'b list =
  let stmts, vals =
    List.fold_left
      (fun (sacc, vacc) item ->
        let s, v = f item in
        (List.rev_append s sacc, v :: vacc))
      ([], []) items
  in
  (List.rev stmts, List.rev vals)

type tctx = { fn : string; params : string list; mutable triggered : bool }

let bind_binds env binds =
  let env, rev =
    List.fold_left
      (fun (env, acc) (src, occ) ->
        let env, js = bind_one env src in
        (env, J.ConstDecl { name = js; init = occ } :: acc))
      (env, []) binds
  in
  (env, List.rev rev)

let bind_params env names =
  let env, rev =
    List.fold_left
      (fun (env, acc) src ->
        let env, js = bind_one env src in
        (env, js :: acc))
      (env, []) names
  in
  (env, List.rev rev)

let accessor_arrow field =
  J.Arrow
    {
      params = [ "r" ];
      body =
        J.ArrowExpr (member (J.Identifier "r") (Data.Located.unwrap field));
    }

let arrow_of_body params stmts =
  let body =
    match stmts with
    | [ J.Return (Some e) ] -> J.ArrowExpr e
    | _ -> J.ArrowBlock stmts
  in
  J.Arrow { params; body }

let rec emit_value env (e : O.Expr.t) : J.stmt list * J.expr =
  match e.expr with
  | O.Expr.Expr_int n -> ([], J.Literal (J.Int n))
  | O.Expr.Expr_float f -> ([], J.Literal (J.Float f))
  | O.Expr.Expr_string s -> ([], J.Literal (J.String s))
  | O.Expr.Expr_char c -> ([], J.Literal (J.String c))
  | O.Expr.Expr_ident name -> ([], jid_env env name)
  | O.Expr.Expr_record_empty -> ([], J.Object [])
  | O.Expr.Expr_record_extend name -> ([], jid_env env name)
  | O.Expr.Expr_record_select name -> ([], jid_env env name)
  | O.Expr.Expr_accessor field -> ([], accessor_arrow field)
  | O.Expr.Expr_access { expr; field } ->
      let s, o = emit_value env expr in
      ( s,
        J.Member
          {
            object_ = o;
            property = J.Identifier (Data.Located.unwrap field);
            computed = false;
          } )
  | O.Expr.Expr_binop { name; operands = a, b } ->
      let sa, ea = emit_value env a in
      let sb, eb = emit_value env b in
      (sa @ sb, binop_expr name ea eb)
  | O.Expr.Expr_constr { name; arguments } ->
      let ss, es = emit_values env arguments in
      (ss, constructor_to_object name es)
  | O.Expr.Expr_record rows ->
      let ss, pairs =
        fold_emit
          (fun { O.Expr.name; value } ->
            let s, v = emit_value env value in
            (s, (name, v)))
          rows
      in
      (ss, J.Object pairs)
  | O.Expr.Expr_list es ->
      let ss, vs = emit_values env es in
      (ss, list_to_cons_cells vs)
  | O.Expr.Expr_apply { fn; arg } -> emit_apply env fn arg
  | O.Expr.Expr_lambda { params; body } -> ([], emit_lambda env params body)
  | O.Expr.Expr_if_then_else { if_exp; then_exp; else_exp } ->
      let sc, ec = emit_value env if_exp in
      let st, et = emit_value env then_exp in
      let se, ee = emit_value env else_exp in
      if st = [] && se = [] then
        (sc, J.Conditional { test = ec; consequent = et; alternate = ee })
      else
        let r = fresh_temp () in
        ( sc
          @ [
              J.VarDecl { name = r; init = None };
              J.If
                {
                  test = ec;
                  consequent = st @ [ assign_stmt r et ];
                  alternate = Some (se @ [ assign_stmt r ee ]);
                };
            ],
          J.Identifier r )
  | O.Expr.Expr_let { binding; body } ->

      let sv, ev = emit_value env binding.bind_body.body in
      let env', name =
        bind_one env (Data.Located.unwrap binding.bind_body.name)
      in
      let sb, eb = emit_value env' body in
      (sv @ [ J.ConstDecl { name; init = ev } ] @ sb, eb)
  | O.Expr.Expr_pattern { expr; pattern_data_items } ->
      let ss, occ, sbind = emit_scrutinee env expr in
      let r = fresh_temp () in
      let chain = emit_match_assign env r occ pattern_data_items in
      ( ss @ sbind @ [ J.VarDecl { name = r; init = None } ] @ chain,
        J.Identifier r )

and emit_values env (es : O.Expr.t list) : J.stmt list * J.expr list =
  fold_emit (emit_value env) es

and emit_scrutinee env (expr : O.Expr.t) : J.stmt list * J.expr * J.stmt list =
  let s, e = emit_value env expr in
  if needs_temp_var e then
    let t = fresh_temp () in
    (s, J.Identifier t, [ J.ConstDecl { name = t; init = e } ])
  else (s, e, [])

and emit_apply env fn arg =
  if is_record_construction fn then emit_record_apply env fn arg
  else
    let callee, args = collect_args [ arg ] fn in
    match (callee.expr, args) with
    | O.Expr.Expr_ident op, [ a1; a2 ] when is_operator env op ->
        let sa, ea = emit_value env a1 in
        let sb, eb = emit_value env a2 in
        (sa @ sb, binop_expr op ea eb)
    | O.Expr.Expr_ident name, _ -> (
        match top_arity env name with
        | Some n -> emit_known_call env (jid_env env name) n args
        | None -> emit_generic env callee args)
    | _ -> emit_generic env callee args

and emit_known_call env callee n args =
  let ss, es = emit_values env args in
  let len = List.length es in
  if len = n then (ss, J.Call { callee; args = es })
  else if len < n then (ss, closure_partial callee es (n - len))
  else
    let firstn, rest = split_at n es in
    (ss, curry_call (J.Call { callee; args = firstn }) rest)

and emit_generic env callee args =
  let sc, ec = emit_value env callee in
  let ss, es = emit_values env args in
  (sc @ ss, curry_call ec es)

and emit_record_apply env fn arg =
  let apply_expr =
    { O.Expr.typ = fn.O.Expr.typ; expr = O.Expr.Expr_apply { fn; arg } }
  in
  match extract_record_fields apply_expr with
  | [] ->
      let sf, ef = emit_value env fn in
      let sa, ea = emit_value env arg in
      (sf @ sa, J.Call { callee = ef; args = [ ea ] })
  | fields ->
      let ss, pairs =
        fold_emit
          (fun (n, v) ->
            let s, e = emit_value env v in
            (s, (n, e)))
          (List.rev fields)
      in
      (ss, J.Object pairs)

and emit_lambda env params body =
  let names =
    List.map (fun (p : O.Expr.expr_lambda_param) -> Data.Located.unwrap p.name) params
  in
  let env, param_names = bind_params env names in

  arrow_of_body param_names (emit_return env None body)

and self_tail_args env tc (e : O.Expr.t) : O.Expr.t list option =
  match e.expr with
  | O.Expr.Expr_apply { fn; arg } -> (
      let callee, args = collect_args [ arg ] fn in
      match callee.expr with
      | O.Expr.Expr_ident name
        when name = tc.fn
             && (not (SMap.mem name env))
             && List.length args = List.length tc.params ->
          Some args
      | _ -> None)
  | _ -> None

and loop_step env tc args =
  let ss, es = emit_values env args in
  let temps = List.map (fun _ -> fresh_temp ()) es in
  let bind = List.map2 (fun t v -> J.ConstDecl { name = t; init = v }) temps es in
  let step =
    List.map2 (fun p t -> assign_stmt p (J.Identifier t)) tc.params temps
  in
  ss @ bind @ step @ [ J.Continue ]

and tail_self_call env tc (e : O.Expr.t) : J.stmt list option =
  match tc with
  | Some tc -> (
      match self_tail_args env tc e with
      | Some args ->
          tc.triggered <- true;
          Some (loop_step env tc args)
      | None -> None)
  | None -> None

and emit_return env tc (e : O.Expr.t) : J.stmt list =
  match tail_self_call env tc e with
  | Some stmts -> stmts
  | None -> (
      match e.expr with
      | O.Expr.Expr_let { binding; body } ->
          let sv, ev = emit_value env binding.bind_body.body in
          let env', name =
            bind_one env (Data.Located.unwrap binding.bind_body.name)
          in
          sv @ [ J.ConstDecl { name; init = ev } ] @ emit_return env' tc body
      | O.Expr.Expr_if_then_else { if_exp; then_exp; else_exp } ->
          let sc, ec = emit_value env if_exp in
          sc
          @ [
              J.If
                {
                  test = ec;
                  consequent = emit_return env tc then_exp;
                  alternate = Some (emit_return env tc else_exp);
                };
            ]
      | O.Expr.Expr_pattern { expr; pattern_data_items } ->
          let ss, occ, sbind = emit_scrutinee env expr in
          ss @ sbind @ emit_match_return env tc occ pattern_data_items
      | _ ->
          let s, ev = emit_value env e in
          s @ [ J.Return (Some ev) ])

and emit_clauses env leaf on_fail occ clauses =
  match clauses with
  | [] -> on_fail
  | { O.Expr.pattern; expr = action } :: rest ->
      let test_opt, binds = match_pattern occ pattern in
      let env', bstmts = bind_binds env binds in
      let stmts = bstmts @ leaf env' action in
      (match test_opt with
      | None -> stmts
      | Some test ->
          [
            J.If
              {
                test;
                consequent = stmts;
                alternate = Some (emit_clauses env leaf on_fail occ rest);
              };
          ])

and emit_match_return env tc (occ : J.expr)
    (clauses : O.Expr.expr_pattern_case list) : J.stmt list =
  match try_switch_return env tc occ clauses with
  | Some stmts -> stmts
  | None ->
      emit_clauses env
        (fun env action -> emit_return env tc action)
        [ J.Return (Some (match_error occ)) ]
        occ clauses

and switch_label (p : O.Pattern.t) =

  match p.O.Pattern.pattern with
  | O.Pattern.P_T_ctor (name, []) when not (is_bool_constructor name) ->
      Some (J.String name)
  | O.Pattern.P_T_int n -> Some (J.Int n)
  | O.Pattern.P_T_str s -> Some (J.String s)
  | O.Pattern.P_T_chr c -> Some (J.String c)
  | _ -> None

and try_switch_return env tc occ clauses =

  let rec classify acc = function
    | [] -> Some (List.rev acc, None)
    | [ { O.Expr.pattern; expr = action } ] when is_wildcard_pat pattern ->
        let binds =
          match pattern.O.Pattern.pattern with
          | O.Pattern.P_T_var name -> [ (name, occ) ]
          | _ -> []
        in
        Some (List.rev acc, Some (binds, action))
    | { O.Expr.pattern; expr = action } :: rest -> (
        match switch_label pattern with
        | Some lbl -> classify ((lbl, action) :: acc) rest
        | None -> None)
  in
  match classify [] clauses with
  | Some ((_ :: _ as cases), default) when List.length cases >= 2 ->
      let case_of (lbl, action) =
        { J.test = Some (J.Literal lbl); consequent = emit_return env tc action }
      in
      let default_case =
        match default with
        | Some (binds, action) ->
            let env', bstmts = bind_binds env binds in
            [ { J.test = None; consequent = bstmts @ emit_return env' tc action } ]
        | None ->
            [
              {
                J.test = None;
                consequent = [ J.Return (Some (match_error occ)) ];
              };
            ]
      in
      Some
        [
          J.Switch
            {
              discriminant = occ;
              cases = List.map case_of cases @ default_case;
            };
        ]
  | _ -> None

and emit_match_assign env (r : string) (occ : J.expr)
    (clauses : O.Expr.expr_pattern_case list) : J.stmt list =
  emit_clauses env
    (fun env action ->
      let sa, ea = emit_value env action in
      sa @ [ assign_stmt r ea ])
    [ assign_stmt r (match_error occ) ]
    occ clauses

let decl_stmts (decl : O.Declaration.t) : J.stmt list =
  let name = sname decl.name in
  match decl.params with
  | [] ->

      let s, e = emit_value SMap.empty decl.body in
      s @ [ J.ConstDecl { name; init = e } ]
  | params ->
      let names =
        List.map (fun (p : O.Declaration.param) -> Data.Located.unwrap p.name) params
      in
      let env, param_names = bind_params SMap.empty names in
      let tc =
        {
          fn = Data.Located.unwrap decl.name;
          params = param_names;
          triggered = false;
        }
      in
      let body = emit_return env (Some tc) decl.body in

      let body =
        if tc.triggered then
          [ J.While { test = J.Literal (J.Bool true); body } ]
        else body
      in
      [ J.ConstDecl { name; init = arrow_of_body param_names body } ]

let program_of_declarations (decls : O.Declaration.t list) : J.program =
  List.concat_map decl_stmts decls

let runtime_prelude =
  "const $$matchError = (value) => {\n\
  \  console.error(\"Pattern match failed for value:\", value);\n\
  \  return null;\n\
   };\n\
   const $$curry = (f, args) => {\n\
  \  const n = f.length === 0 ? 1 : f.length;\n\
  \  if (args.length === n) return f(...args);\n\
  \  if (args.length < n) return (...more) => $$curry(f, [...args, ...more]);\n\
  \  return $$curry(f(...args.slice(0, n)), args.slice(n));\n\
   };\n"

let constructor_decls (constructors : (string * int) list) : J.stmt list =
  constructors
  |> List.filter (fun (name, _) ->
         not (is_bool_constructor name || is_unit_constructor name))
  |> List.map (fun (name, arity) ->

         if arity = 0 then
           J.ConstDecl { name = sanitize name; init = constructor_to_object name [] }
         else
           let params = List.init arity (fun i -> "_" ^ string_of_int i) in
           let args = List.map (fun p -> J.Identifier p) params in
           J.ConstDecl
             {
               name = sanitize name;
               init =
                 J.Arrow
                   {
                     params;
                     body = J.ArrowExpr (constructor_to_object name args);
                   };
             })

let program_with_helpers ?(constructors = []) (decls : O.Declaration.t list) :
    J.program =
  reset_names ();
  List.iter
    (fun n -> reserve_name (sanitize n))
    [ "$$matchError"; "$$curry"; "console" ];
  List.iter (fun (decl : O.Declaration.t) -> reserve_name (sname decl.name)) decls;
  List.iter (fun (name, _) -> reserve_name (sanitize name)) constructors;
  List.iter
    (fun (decl : O.Declaration.t) ->
      Hashtbl.replace arities
        (Data.Located.unwrap decl.name)
        (List.length decl.params))
    decls;
  List.iter (fun (name, ar) -> Hashtbl.replace arities name ar) constructors;
  constructor_decls constructors @ program_of_declarations decls
