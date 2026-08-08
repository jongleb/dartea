module J = Js_ast
module O = Optimized
module DT = After_typed.Exhaustive.Decision_tree
module Occ = After_typed.Exhaustive.Occurrence

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

let ctor_siblings : (string, (string * int) list) Hashtbl.t = Hashtbl.create 64

let ctor_siblings_of name = Hashtbl.find_opt ctor_siblings name

let js_arity : (string, int) Hashtbl.t = Hashtbl.create 64

let reset_names () =
  Hashtbl.clear name_counts;
  Hashtbl.clear ctor_siblings;
  Hashtbl.clear js_arity;
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
  | "++" -> Some J.Plus
  | "==" -> Some J.StrictEqual
  | "!=" -> Some J.StrictNotEqual
  | "/=" -> Some J.StrictNotEqual
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

let is_inline_constructor name =
  is_bool_constructor name || is_unit_constructor name

let is_tag_omitted name =
  match ctor_siblings_of name with
  | Some siblings -> (
      match List.filter (fun (_, arity) -> arity >= 1) siblings with
      | [ (only, _) ] -> only = name
      | _ -> false)
  | None -> false

let payload_fields js_arguments =
  List.mapi (fun i a -> ("_" ^ string_of_int i, a)) js_arguments

let constructor_to_object name js_arguments =
  if is_bool_constructor name then bool_literal name
  else if is_unit_constructor name then J.Literal J.Null
  else if js_arguments = [] then J.Literal (J.String name)
  else if is_tag_omitted name then J.Object (payload_fields js_arguments)
  else
    J.Object (("TAG", J.Literal (J.String name)) :: payload_fields js_arguments)

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

let js_eq left lit = J.Binary { left; op = J.StrictEqual; right = J.Literal lit }

let js_ne_zero occ =
  J.Binary { left = occ; op = J.StrictNotEqual; right = J.Literal (J.Int 0) }

let js_is_object occ =
  J.Binary
    {
      left = J.Unary { op = J.Typeof; arg = occ };
      op = J.StrictEqual;
      right = J.Literal (J.String "object");
    }

let occ_expr root (o : Occ.t) : J.expr =
  List.fold_left
    (fun e step ->
      match step with
      | Occ.Payload i -> member e ("_" ^ string_of_int i)
      | Occ.Index i -> indexed_member e i
      | Occ.Field f -> member e f
      | Occ.Hd -> member e "hd"
      | Occ.Tl -> member e "tl")
    root o

let ctor_literal name =
  if is_bool_constructor name then J.Bool (name = "True")
  else J.String name

let test_expr occ_e (test : DT.test) : J.expr =
  match test with
  | DT.Test_ctor name -> js_eq occ_e (ctor_literal name)
  | DT.Test_tag name ->
      if is_tag_omitted name then js_is_object occ_e
      else js_eq (member occ_e "TAG") (J.String name)
  | DT.Test_int n -> js_eq occ_e (J.Int n)
  | DT.Test_str s -> js_eq occ_e (J.String s)
  | DT.Test_chr c -> js_eq occ_e (J.String c)
  | DT.Test_nil -> js_eq occ_e (J.Int 0)
  | DT.Test_cons -> js_ne_zero occ_e

let switch_key (test : DT.test) : ([ `Value | `Tag ] * J.literal) option =
  match test with
  | DT.Test_tag n when not (is_tag_omitted n) -> Some (`Tag, J.String n)
  | DT.Test_tag _ -> None
  | DT.Test_ctor n when not (is_bool_constructor n) -> Some (`Value, J.String n)
  | DT.Test_int n -> Some (`Value, J.Int n)
  | DT.Test_str s -> Some (`Value, J.String s)
  | DT.Test_chr c -> Some (`Value, J.String c)
  | DT.Test_ctor _ | DT.Test_nil | DT.Test_cons -> None

let switch_plan occ_e (branches : (DT.test * DT.t) list) :
    (J.expr * (J.literal * DT.t) list) option =
  let keyed =
    List.map (fun (t, tr) -> (switch_key t, tr)) branches
  in
  let same_kind kind =
    List.for_all
      (fun (k, _) -> match k with Some (k', _) -> k' = kind | None -> false)
      keyed
  in
  let cases_of () =
    List.map
      (fun (k, tr) ->
        match k with Some (_, lit) -> (lit, tr) | None -> assert false)
      keyed
  in
  if keyed <> [] && same_kind `Tag then Some (member occ_e "TAG", cases_of ())
  else if keyed <> [] && same_kind `Value then Some (occ_e, cases_of ())
  else None

let match_error occ =
  J.Call { callee = J.Identifier "$$matchError"; args = [ occ ] }

let assign_stmt r e = J.ExprStmt (J.Assignment { left = J.Identifier r; right = e })

let binop_expr name ea eb =
  match binop_of_string name with
  | Some J.Divide ->
      J.Call
        {
          callee = J.Identifier "Math.trunc";
          args = [ J.Binary { left = ea; op = J.Divide; right = eb } ];
        }
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

let rec type_arity (t : O.Type.t) =
  match t with O.Type.TFun (_, result) -> 1 + type_arity result | _ -> 0

let closure_partial callee args missing =
  let rparams = List.init missing (fun _ -> fresh_temp ()) in
  let rargs = List.map (fun p -> J.Identifier p) rparams in
  J.Arrow
    {
      params = rparams;
      body = J.ArrowExpr (J.Call { callee; args = args @ rargs });
    }

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

module DS = After_typed.Decision_share

let thunk_names plan =
  List.map
    (fun (id, _) -> (id, fresh_js ("$dt" ^ string_of_int id)))
    (DS.shared plan)

let rec lower env root ~terminating ~leaf ~fail ~sink ~plan ~tnames
    (tree : DT.t) : J.stmt list =
  match DS.id_of plan tree with
  | Some id ->
      sink
        (J.Call
           { callee = J.Identifier (List.assoc id tnames); args = [] })
  | None -> lower_node env root ~terminating ~leaf ~fail ~sink ~plan ~tnames tree

and lower_node env root ~terminating ~leaf ~fail ~sink ~plan ~tnames
    (tree : DT.t) : J.stmt list =
  match tree with
  | DT.Fail -> fail
  | DT.Leaf { action; bindings } ->
      let jbinds = List.map (fun (v, o) -> (v, occ_expr root o)) bindings in
      let env', bstmts = bind_binds env jbinds in
      bstmts @ leaf env' action
  | DT.Switch { occurrence; branches; default } -> (
      let occ_e = occ_expr root occurrence in
      let go tr =
        lower env root ~terminating ~leaf ~fail ~sink ~plan ~tnames tr
      in
      match (if terminating then switch_plan occ_e branches else None) with
      | Some (disc, cases) when List.length cases >= 2 ->
          let default_case =
            match default with
            | Some t -> [ { J.test = None; consequent = go t } ]
            | None -> []
          in
          let js_cases =
            List.map
              (fun (lit, tr) ->
                { J.test = Some (J.Literal lit); consequent = go tr })
              cases
          in
          [ J.Switch { discriminant = disc; cases = js_cases @ default_case } ]
      | _ ->
          let rec build = function
            | [] -> ( match default with Some t -> go t | None -> fail)
            | [ (_, tr) ] when default = None -> go tr
            | (test, tr) :: rest ->
                [
                  J.If
                    {
                      test = test_expr occ_e test;
                      consequent = go tr;
                      alternate = Some (build rest);
                    };
                ]
          in
          build branches)

let shared_thunks env root ~plan ~tnames clause_expr =
  let sink e = [ J.Return (Some e) ] in
  let leaf env action =
    let sa, ea = clause_expr env action in
    sa @ [ J.Return (Some ea) ]
  in
  List.map
    (fun (id, sub) ->
      let body =
        lower_node env root ~terminating:true ~leaf
          ~fail:(sink (match_error root)) ~sink ~plan ~tnames sub
      in
      J.ConstDecl { name = List.assoc id tnames; init = arrow_of_body [] body })
    (DS.shared plan)

let rec emit_value env (e : O.Expr.t) : J.stmt list * J.expr =
  match e.expr with
  | O.Expr.Expr_int n -> ([], J.Literal (J.Int n))
  | O.Expr.Expr_float f -> ([], J.Literal (J.Float f))
  | O.Expr.Expr_string s -> ([], J.Literal (J.String s))
  | O.Expr.Expr_char c -> ([], J.Literal (J.String c))
  | O.Expr.Expr_ident name when is_inline_constructor name ->
      ([], constructor_to_object name [])
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

      let env', name =
        bind_one env (Data.Located.unwrap binding.bind_body.name)
      in
      let sv, ev = emit_value env' binding.bind_body.body in
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
    | O.Expr.Expr_ident name, _ ->
        let known =
          if SMap.mem name env then None else Hashtbl.find_opt js_arity name
        in
        let n =
          match known with Some n -> n | None -> type_arity callee.O.Expr.typ
        in
        if n >= 1 then emit_known_call env (jid_env env name) n args
        else emit_generic env callee args
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
          let env', name =
            bind_one env (Data.Located.unwrap binding.bind_body.name)
          in
          let sv, ev = emit_value env' binding.bind_body.body in
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

and match_tree clauses =
  let patterns =
    List.map (fun (c : O.Expr.expr_pattern_case) -> c.O.Expr.pattern) clauses
  in
  (After_typed.Exhaustive.build ctor_siblings_of patterns, Array.of_list clauses)

and trivial_action (e : O.Expr.t) =
  match e.O.Expr.expr with
  | O.Expr.Expr_int _ | O.Expr.Expr_float _ | O.Expr.Expr_string _
  | O.Expr.Expr_char _ | O.Expr.Expr_ident _ ->
      true
  | O.Expr.Expr_constr { arguments = []; _ } -> true
  | _ -> false

and shareable (clause_arr : O.Expr.expr_pattern_case array) (tree : DT.t) =
  match tree with
  | DT.Switch _ -> true
  | DT.Leaf { action; _ } -> not (trivial_action clause_arr.(action).expr)
  | DT.Fail -> false

and emit_match_return env tc (occ : J.expr)
    (clauses : O.Expr.expr_pattern_case list) : J.stmt list =
  let tree, clause_arr = match_tree clauses in
  let plan = DS.analyze ~shareable:(shareable clause_arr) tree in
  let clause_expr env action =
    emit_value env clause_arr.(action).O.Expr.expr
  in
  let leaf env action = emit_return env tc clause_arr.(action).O.Expr.expr in
  let sink e = [ J.Return (Some e) ] in
  let tnames = thunk_names plan in
  shared_thunks env occ ~plan ~tnames clause_expr
  @ lower env occ ~terminating:true ~leaf
      ~fail:[ J.Return (Some (match_error occ)) ]
      ~sink ~plan ~tnames tree

and emit_match_assign env (r : string) (occ : J.expr)
    (clauses : O.Expr.expr_pattern_case list) : J.stmt list =
  let tree, clause_arr = match_tree clauses in
  let plan = DS.analyze ~shareable:(shareable clause_arr) tree in
  let clause_expr env action =
    emit_value env clause_arr.(action).O.Expr.expr
  in
  let leaf env action =
    let sa, ea = emit_value env clause_arr.(action).O.Expr.expr in
    sa @ [ assign_stmt r ea ]
  in
  let sink e = [ assign_stmt r e ] in
  let tnames = thunk_names plan in
  shared_thunks env occ ~plan ~tnames clause_expr
  @ lower env occ ~terminating:false ~leaf
      ~fail:[ assign_stmt r (match_error occ) ]
      ~sink ~plan ~tnames tree

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

let decl_js_arity (decl : O.Declaration.t) =
  match decl.params with
  | [] -> (
      match decl.body.expr with
      | O.Expr.Expr_lambda { params; _ } -> List.length params
      | _ -> 0)
  | params -> List.length params

let constructor_decls (constructors : (string * int) list) : J.stmt list =
  constructors
  |> List.filter (fun (name, _) -> not (is_inline_constructor name))
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

let program_with_helpers ?(constructors = []) ?(siblings = [])
    (decls : O.Declaration.t list) : J.program =
  reset_names ();
  List.iter (fun n -> reserve_name (sanitize n)) Runtime.reserved;
  List.iter (fun (decl : O.Declaration.t) -> reserve_name (sname decl.name)) decls;
  List.iter (fun (name, _) -> reserve_name (sanitize name)) constructors;
  List.iter
    (fun (name, sibs) -> Hashtbl.replace ctor_siblings name sibs)
    siblings;
  List.iter (fun (name, arity) -> Hashtbl.replace js_arity name arity) constructors;
  List.iter
    (fun (decl : O.Declaration.t) ->
      Hashtbl.replace js_arity (Data.Located.unwrap decl.name) (decl_js_arity decl))
    decls;
  constructor_decls constructors @ program_of_declarations decls

let mentions body name =
  let nl = String.length name and hl = String.length body in
  let rec go i = i + nl <= hl && (String.sub body i nl = name || go (i + 1)) in
  go 0

let emit ?(constructors = []) ?(siblings = []) (decls : O.Declaration.t list) :
    string =
  let body =
    Js_to_string.program_to_string
      (program_with_helpers ~constructors ~siblings decls)
  in
  let user = List.map (fun (d : O.Declaration.t) -> Data.Located.unwrap d.name) decls in
  let needed (name, _) = (not (List.mem name user)) && mentions body name in
  let defs = List.filter_map (fun b -> if needed b then Some (snd b) else None) Runtime.builtins in
  Runtime.core ^ String.concat "" (List.map (fun d -> d ^ "\n") defs) ^ body
