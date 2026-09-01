module J = Ast
module O = Optimized
module DT = After_typed.Exhaustive.Decision_tree
module Occ = After_typed.Exhaustive.Occurrence
module DS = After_typed.Exhaustive.Share
module Scope = Data.Name.Map

type env = Env.t
type arity = Lower.arity = Exactly of int | At_least of int

let runtime_module_name = Names.runtime_module
let module_ident = Names.module_ident
let js_of_name = Names.of_name
let extension = "mjs"
let module_suffix = "." ^ extension
let module_file module_name = module_name ^ module_suffix

let is_record_construction (expr_node : O.Expr.t) =
  let head, _ = O.Expr.spine expr_node in
  Option.is_some (O.Expr.record_extend_of head)

let rec extract_record_fields (expr_node : O.Expr.t) : (string * O.Expr.t) list
    =
  let head, arguments = O.Expr.spine expr_node in
  match (O.Expr.record_extend_of head, arguments) with
  | Some field, [ value ] -> [ (field, value) ]
  | Some field, [ value; rest ] -> (field, value) :: extract_record_fields rest
  | Some _, _ | None, _ -> []

let applied_spine ~fn ~arg =
  let callee, arguments = O.Expr.spine fn in
  (callee, arguments @ [ arg ])

let rec list_to_cons_cells = function
  | [] -> J.int 0
  | hd :: tl ->
      J.Object [ J.Field (Runtime.head, hd); J.Field (Runtime.tail, list_to_cons_cells tl) ]

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

type tctx = {
  fn : Data.Name.t;
  js : string;
  params : string list;
  mutable triggered : bool;
}

let accessor_arrow field =
  J.Arrow
    {
      params = [ "r" ];
      body = J.ArrowExpr (J.member (J.Identifier "r") (Data.Located.unwrap field));
    }



let rec emit_value env (e : O.Expr.t) : J.stmt list * J.expr =
  let statements, expression = emit_raw env e in
  ( statements,
    coerce env expression
      ~expected:(Lower.arity_of_type e.O.Expr.typ)
      ~actual:(emitted_arity env e) )

and coerce env expression ~expected ~actual =
  match (expected, actual) with
  | Exactly wanted, Exactly given
    when wanted <> given && wanted >= 1 && given >= 1 ->
      let params = List.init wanted (fun _ -> Env.temp env) in
      let arguments = List.map (fun p -> J.Identifier p) params in
      let call_in_two_steps () =
        let first, extra = Lower.split_at given arguments in
        J.call (J.call expression first) extra
      in
      let body =
        if given < wanted then call_in_two_steps ()
        else Lower.closure_partial env expression arguments (given - wanted)
      in
      J.Arrow { params; body = J.ArrowExpr body }
  | (Exactly _ | At_least _), (Exactly _ | At_least _) -> expression

and emitted_arity env (e : O.Expr.t) : arity =
  match e.expr with
  | O.Expr.Expr_lambda { params; _ } -> Exactly (List.length params)
  | O.Expr.Expr_kernel (Kernel_value kernel) ->
      Exactly (Data.Kernel.arity kernel)
  | O.Expr.Expr_constr { name; arguments } ->
      let given_count = List.length arguments in
      begin
        match Env.declared_arity env name with
        | Some n when n > given_count -> Exactly (n - given_count)
        | Some _ | None -> Lower.arity_of_type e.typ
      end
  | O.Expr.Expr_ident _ -> callee_arity env e
  | O.Expr.Expr_apply { fn; _ } when is_record_construction fn -> Exactly 0
  | O.Expr.Expr_apply { fn; arg } ->
      let callee, args = applied_spine ~fn ~arg in
      let given_count = List.length args in
      begin
        match callee_arity env callee with
        | Exactly n when n > given_count -> Exactly (n - given_count)
        | Exactly n when n >= 1 -> Lower.arity_of_type e.typ
        | Exactly _ | At_least _ -> At_least 0
      end
  | O.Expr.Expr_binop _ | O.Expr.Expr_let _ | O.Expr.Expr_if_then_else _
  | O.Expr.Expr_record _ | O.Expr.Expr_record_update _
  | O.Expr.Expr_pattern _ | O.Expr.Expr_accessor _
  | O.Expr.Expr_access _ | O.Expr.Expr_record_extend _
  | O.Expr.Expr_record_select _ | O.Expr.Expr_record_empty | O.Expr.Expr_unit
  | O.Expr.Expr_kernel _ | O.Expr.Expr_char _ | O.Expr.Expr_string _
  | O.Expr.Expr_int _ | O.Expr.Expr_float _ | O.Expr.Expr_list _
  | O.Expr.Expr_cons _ | O.Expr.Expr_tuple _ ->
      Lower.arity_of_type e.typ

and callee_arity env (callee : O.Expr.t) : arity =
  Option.bind (O.Expr.ident_of callee) (Env.declared_arity env)
  |> Option.fold ~none:(Lower.arity_of_type callee.typ) ~some:(fun n -> Exactly n)

and emit_raw env (e : O.Expr.t) : J.stmt list * J.expr =
  match e.expr with
  | O.Expr.Expr_int n -> ([], J.int n)
  | O.Expr.Expr_float f -> ([], J.Literal (J.Float f))
  | O.Expr.Expr_string s -> ([], J.string s)
  | O.Expr.Expr_char c -> ([], J.string c)
  | O.Expr.Expr_ident name when Names.is_inline_constructor name ->
      ([], Names.constructor_to_object env.Env.names name [])
  | O.Expr.Expr_ident name -> ([], Env.jid_env env name)
  | O.Expr.Expr_record_empty -> ([], J.Object [])
  | O.Expr.Expr_unit -> ([], J.Literal J.Null)
  | O.Expr.Expr_kernel (Kernel_value kernel) -> ([], Of_kernel.value kernel)
  | O.Expr.Expr_kernel (Kernel_unary { kernel; argument }) ->
      let statements, subject = emit_value env argument in
      (statements, Of_kernel.unary_operation kernel subject)
  | O.Expr.Expr_kernel (Kernel_binary { kernel; left; right }) ->
      let left_statements, left = emit_value env left in
      let right_statements, right = emit_value env right in
      ( left_statements @ right_statements,
        Of_kernel.binary_operation kernel left right )
  | O.Expr.Expr_record_extend name -> ([], Env.jid_env env (Data.Name.local name))
  | O.Expr.Expr_record_select name -> ([], Env.jid_env env (Data.Name.local name))
  | O.Expr.Expr_accessor field -> ([], accessor_arrow field)
  | O.Expr.Expr_access { expr; field } ->
      let s, o = emit_value env expr in
      (s, J.member o (Data.Located.unwrap field))
  | O.Expr.Expr_binop { name; operands = a, b } ->
      let sa, ea = emit_value env a in
      let sb, eb = emit_value env b in
      let bindings, lowered =
        Lower.lower_binary env ~operand:a.O.Expr.typ name ea eb
      in
      (sa @ sb @ bindings, lowered)
  | O.Expr.Expr_constr { name; arguments } ->
      let ss, es = emit_values env arguments in
      (ss, Names.constructor_to_object env.Env.names name es)
  | O.Expr.Expr_record rows ->
      let ss, members = emit_fields env rows in
      (ss, J.Object members)
  | O.Expr.Expr_list es ->
      let ss, vs = emit_values env es in
      (ss, list_to_cons_cells vs)
  | O.Expr.Expr_cons { head; tail } ->
      let sh, eh = emit_value env head in
      let st, et = emit_value env tail in
      (sh @ st, J.Object [ J.Field (Runtime.head, eh); J.Field (Runtime.tail, et) ])
  | O.Expr.Expr_tuple items ->
      let ss, vs = emit_values env items in
      (ss, J.Array vs)
  | O.Expr.Expr_record_update { record; fields } ->
      emit_record_update env e.typ record fields
  | O.Expr.Expr_apply { fn; arg } -> begin
      match emit_block env e with
      | Some found -> found
      | None -> emit_apply env fn arg
    end
  | O.Expr.Expr_lambda { params; body } -> ([], emit_lambda env params body)
  | O.Expr.Expr_if_then_else { if_exp; then_exp; else_exp } ->
      let sc, ec = emit_value env if_exp in
      let st, et = emit_value env then_exp in
      let se, ee = emit_value env else_exp in
      if List.is_empty st && List.is_empty se then
        (sc, J.Conditional { test = ec; consequent = et; alternate = ee })
      else
        let r = Env.temp env in
        ( sc
          @ [
              J.VarDecl { name = r; init = None };
              J.If
                {
                  test = ec;
                  consequent = st @ [ J.assign r et ];
                  alternate = Some (se @ [ J.assign r ee ]);
                };
            ],
          J.Identifier r )
  | O.Expr.Expr_let { binding; body } ->
      let env', bound = emit_binding env binding in
      let sb, eb = emit_value env' body in
      (bound @ sb, eb)
  | O.Expr.Expr_pattern { expr; pattern_data_items } ->
      let ss, occ, sbind = emit_scrutinee env expr in
      let r = Env.temp env in
      let chain = emit_match_assign env r occ pattern_data_items in
      ( ss @ sbind @ [ J.VarDecl { name = r; init = None } ] @ chain,
        J.Identifier r )

and emit_values env (es : O.Expr.t list) : J.stmt list * J.expr list =
  fold_emit (emit_value env) es

and emit_block env (e : O.Expr.t) : (J.stmt list * J.expr) option =
  Block_emit.emit ~emit_value ~emit_values env e

and emit_fields env (rows : O.Expr.expr_record_row list) :
    J.stmt list * J.object_member list =
  fold_emit
    (fun { O.Expr.name; value } ->
      let s, v = emit_value env value in
      (s, J.Field (name, v)))
    rows

and emit_binding env (binding : O.Expr.expr_let_binding) =
  let src = Data.Name.local (Data.Located.unwrap binding.bind_body.name) in
  let env', name = Env.bind_one env src in
  let bound = binding.bind_body.body in
  let recursive =
    Data.Name.Set.mem src (O.Expr.free_variables ~bound:Data.Name.Set.empty bound)
  in
  match bound.expr with
  | O.Expr.Expr_lambda { params; body } when recursive ->
      let env_fn, param_names = bind_lambda_params env' params in
      let tc = { fn = src; js = name; params = param_names; triggered = false } in
      let stmts = loop_body env_fn tc body in
      (env', [ J.ConstDecl { name; init = J.arrow_of_body param_names stmts } ])
  | _ ->
      let sv, ev = emit_value env' bound in
      (env', sv @ [ J.ConstDecl { name; init = ev } ])

and record_labels (typ : O.Type.t) : string list option =
  let rec row (t : O.Type.t) =
    match t with
    | O.Type.TRowExtend (label, _, rest) -> Option.map (List.cons label) (row rest)
    | O.Type.TRowEmpty -> Some []
    | O.Type.TVar _ | O.Type.TInt | O.Type.TFloat | O.Type.TChar | O.Type.TBool
    | O.Type.TStr | O.Type.TUnit | O.Type.TFun _ | O.Type.TTup _
    | O.Type.TCustom _ | O.Type.TRecord _ ->
        None
  in
  match typ with O.Type.TRecord inner -> row inner | _ -> None

and emit_record_update env typ (record : O.Expr.t) (fields : O.Expr.expr_record_row list) =
  let sr, er, share = emit_scrutinee env record in
  let ss, members = emit_fields env fields in
  let field_name (member : J.object_member) =
    match member with J.Field (name, _) -> name | J.Spread _ -> ""
  in
  let written = List.map field_name members in
  match record_labels typ with
  | Some labels ->
      let carried =
        labels
        |> List.filter (fun label -> not (List.mem label written))
        |> List.map (fun label -> J.Field (label, J.member er label))
      in
      (sr @ share @ ss, J.Object (carried @ members))
  | None -> (sr @ ss @ share, J.Object (J.Spread er :: members))

and emit_scrutinee env (expr : O.Expr.t) : J.stmt list * J.expr * J.stmt list =
  let s, e = emit_value env expr in
  if Lower.needs_temp_var e then
    let t = Env.temp env in
    (s, J.Identifier t, [ J.ConstDecl { name = t; init = e } ])
  else (s, e, [])

and port_kernel (platform : Data.Kernel.platform) direction =
  Data.Name.equal platform.name
    (Data.Kernel.written_as Data.Kernel.Port.module_name
       (Data.Kernel.Port.string_of_direction direction))

and emit_port_outgoing env kernel port_name (payload : O.Expr.t) =
  let sn, en = emit_value env port_name in
  let sp, ep = emit_value env payload in
  let ep =
    match Flags.encoder payload.O.Expr.typ with
    | Ok enc -> Flags.encoded enc ep
    | Error _ -> ep
  in
  (sn @ sp, J.call (Of_kernel.value kernel) [ en; ep ])

and port_where (port_name : O.Expr.t) =
  match port_name.O.Expr.expr with
  | O.Expr.Expr_string name -> Runtime.port_where name
  | _ -> Runtime.port_label

and emit_port_incoming env kernel (port_name : O.Expr.t) (tagger : O.Expr.t) =
  let sn, en = emit_value env port_name in
  let st, et = emit_value env tagger in
  let checked =
    match O.Type.head tagger.O.Expr.typ with
    | O.Type.TFun (inside, _) -> Result.to_option (Flags.decoder inside)
    | _ -> None
  in
  let et =
    match checked with
    | None -> et
    | Some decode ->
        let raw = J.Identifier Runtime.raw in
        let where = J.string (port_where port_name) in
        J.Arrow
          {
            params = [ Runtime.raw ];
            body = J.ArrowExpr (J.call et [ J.call decode [ raw; where ] ]);
          }
  in
  (sn @ st, J.call (Of_kernel.value kernel) [ en; et ])

and emit_operator env callee (args : O.Expr.t list) =
  match args with
  | [ left; right ] ->
      let lowered =
        Option.bind (O.Expr.ident_of callee) (fun name ->
            Lower.lower_full_call env name ~operand:left.O.Expr.typ)
      in
      Option.map
        (fun lower ->
          let sa, ea = emit_value env left in
          let sb, eb = emit_value env right in
          let bindings, made = lower ea eb in
          (sa @ sb @ bindings, made))
        lowered
  | _ -> None

and port_call (callee : O.Expr.t) args =
  match (callee.expr, args) with
  | ( O.Expr.Expr_kernel (Kernel_value (Data.Kernel.Platform platform as kernel)),
      [ port_name; carried ] ) ->
      let directed direction =
        if port_kernel platform direction then Some (direction, kernel, port_name, carried)
        else None
      in
      begin
        match directed Data.Kernel.Port.Outgoing with
        | Some found -> Some found
        | None -> directed Data.Kernel.Port.Incoming
      end
  | _, _ -> None

and emit_apply env fn arg =
  if is_record_construction fn then emit_record_apply env fn arg
  else
    let callee, args = applied_spine ~fn ~arg in
    match emit_operator env callee args with
    | Some found -> found
    | None -> begin
        match port_call callee args with
        | Some (Data.Kernel.Port.Outgoing, kernel, port_name, payload) ->
            emit_port_outgoing env kernel port_name payload
        | Some (Data.Kernel.Port.Incoming, kernel, port_name, tagger) ->
            emit_port_incoming env kernel port_name tagger
        | None -> emit_call env callee args
      end

and emit_call env (callee : O.Expr.t) args =
  match (callee.expr, args) with
  | O.Expr.Expr_kernel (Kernel_value kernel), _ ->
      let arity = Data.Kernel.arity kernel in
      emit_known_call env (Of_kernel.value kernel) ~arity
        ~result_type:
          (O.Type.result_after ~applied:arity callee.O.Expr.typ)
        args
  | O.Expr.Expr_ident name, _ -> begin
      match Env.declared_arity env name with
      | Some n when n >= 1 ->
          emit_known_call env (Env.jid_env env name) ~arity:n
            ~result_type:
              (O.Type.result_after ~applied:n callee.O.Expr.typ)
            args
      | Some _ | None -> emit_generic env callee args
    end
  | ( ( O.Expr.Expr_constr _ | O.Expr.Expr_binop _ | O.Expr.Expr_let _
      | O.Expr.Expr_if_then_else _ | O.Expr.Expr_record _
      | O.Expr.Expr_record_update _ | O.Expr.Expr_apply _
      | O.Expr.Expr_pattern _ | O.Expr.Expr_accessor _
      | O.Expr.Expr_access _ | O.Expr.Expr_record_extend _
      | O.Expr.Expr_record_select _ | O.Expr.Expr_record_empty
      | O.Expr.Expr_unit
      | O.Expr.Expr_kernel (Kernel_unary _ | Kernel_binary _)
      | O.Expr.Expr_lambda _ | O.Expr.Expr_char _ | O.Expr.Expr_string _
      | O.Expr.Expr_int _ | O.Expr.Expr_float _ | O.Expr.Expr_list _
      | O.Expr.Expr_cons _ | O.Expr.Expr_tuple _ ),
      _ ) ->
      emit_generic env callee args

and emit_known_call env callee ~arity ~result_type args =
  let statements, arguments = emit_values env args in
  apply env callee ~arity ~result_type ~statements ~arguments

and apply env callee ~arity ~result_type ~statements ~arguments =
  let given_count = List.length arguments in
  if given_count = arity then (statements, J.call callee arguments)
  else if given_count < arity then
    (statements, Lower.closure_partial env callee arguments (arity - given_count))
  else
    let first, extra = Lower.split_at arity arguments in
    let head_call = J.call callee first in
    match Lower.arity_of_type result_type with
    | Exactly n when n >= 1 ->
        apply env head_call ~arity:n
          ~result_type:(O.Type.result_after ~applied:n result_type)
          ~statements ~arguments:extra
    | Exactly _ | At_least _ -> (statements, Lower.curry_call head_call extra)

and emit_generic env callee args =
  let sc, ec = emit_value env callee in
  let ss, es = emit_values env args in
  match Lower.arity_of_type callee.O.Expr.typ with
  | Exactly arity when arity >= 1 ->
      let bound, target =
        if List.length es < arity && Lower.needs_temp_var ec then
          let t = Env.temp env in
          ([ J.ConstDecl { name = t; init = ec } ], J.Identifier t)
        else ([], ec)
      in
      let statements, expression =
        apply env target ~arity
          ~result_type:(O.Type.result_after ~applied:arity callee.O.Expr.typ)
          ~statements:ss ~arguments:es
      in
      (sc @ bound @ statements, expression)
  | Exactly _ | At_least _ -> (sc @ ss, Lower.curry_call ec es)

and emit_record_apply env fn arg =
  let apply_expr =
    { O.Expr.typ = fn.O.Expr.typ; expr = O.Expr.Expr_apply { fn; arg } }
  in
  match extract_record_fields apply_expr with
  | [] ->
      let sf, ef = emit_value env fn in
      let sa, ea = emit_value env arg in
      (sf @ sa, J.call ef [ ea ])
  | fields ->
      let ss, members =
        fold_emit
          (fun (n, v) ->
            let s, e = emit_value env v in
            (s, J.Field (n, e)))
          (List.rev fields)
      in
      (ss, J.Object members)

and bind_lambda_params env (params : O.Expr.expr_lambda_param list) =
  let names =
    List.map
      (fun (p : O.Expr.expr_lambda_param) ->
        Data.Name.local (Data.Located.unwrap p.name))
      params
  in
  Env.bind_params env names

and emit_lambda env params body =
  let env, param_names = bind_lambda_params env params in
  J.arrow_of_body param_names (emit_return env None body)

and loop_body env tc body =
  let stmts = emit_return env (Some tc) body in
  if tc.triggered then [ J.While { test = J.bool true; body = stmts } ] else stmts

and self_tail_args env tc (e : O.Expr.t) : O.Expr.t list option =
  let callee, args = O.Expr.spine e in
  let unshadowed name =
    match Scope.find_opt name env.Env.scope with
    | None -> true
    | Some js -> String.equal js tc.js
  in
  let calls_itself name = Data.Name.equal name tc.fn && unshadowed name in
  let saturates = List.length args = List.length tc.params in
  match O.Expr.ident_of callee with
  | Some name when calls_itself name && saturates && not (List.is_empty args) -> Some args
  | Some _ | None -> None

and loop_step env tc args =
  let ss, es = emit_values env args in
  let temps = List.map (fun _ -> Env.temp env) es in
  let bind = List.map2 (fun t v -> J.ConstDecl { name = t; init = v }) temps es in
  let step = List.map2 (fun p t -> J.assign p (J.Identifier t)) tc.params temps in
  ss @ bind @ step @ [ J.Continue ]

and tail_self_call env tc (e : O.Expr.t) : J.stmt list option =
  match tc with
  | Some tc -> begin
      match self_tail_args env tc e with
      | Some args ->
          tc.triggered <- true;
          Some (loop_step env tc args)
      | None -> None
    end
  | None -> None

and emit_return env tc (e : O.Expr.t) : J.stmt list =
  match tail_self_call env tc e with
  | Some stmts -> stmts
  | None -> begin
      match e.expr with
      | O.Expr.Expr_let { binding; body } ->
          let env', bound = emit_binding env binding in
          bound @ emit_return env' tc body
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
      | O.Expr.Expr_constr _ | O.Expr.Expr_binop _ | O.Expr.Expr_record _
      | O.Expr.Expr_record_update _ | O.Expr.Expr_apply _ | O.Expr.Expr_ident _
      | O.Expr.Expr_accessor _ | O.Expr.Expr_access _
      | O.Expr.Expr_record_extend _ | O.Expr.Expr_record_select _
      | O.Expr.Expr_record_empty | O.Expr.Expr_unit | O.Expr.Expr_kernel _
      | O.Expr.Expr_lambda _ | O.Expr.Expr_char _ | O.Expr.Expr_string _
      | O.Expr.Expr_int _ | O.Expr.Expr_float _ | O.Expr.Expr_list _
      | O.Expr.Expr_cons _ | O.Expr.Expr_tuple _ ->
          let s, ev = emit_value env e in
          s @ [ J.Return (Some ev) ]
    end

and match_tree env clauses =
  let patterns =
    List.map (fun (c : O.Expr.expr_pattern_case) -> c.O.Expr.pattern) clauses
  in
  ( After_typed.Exhaustive.build (Names.siblings_of env.Env.names) patterns,
    Array.of_list clauses )

and trivial_action (e : O.Expr.t) =
  match e.O.Expr.expr with
  | O.Expr.Expr_int _ | O.Expr.Expr_float _ | O.Expr.Expr_string _
  | O.Expr.Expr_char _ | O.Expr.Expr_ident _ ->
      true
  | O.Expr.Expr_constr { arguments = []; _ } -> true
  | O.Expr.Expr_constr _ | O.Expr.Expr_binop _ | O.Expr.Expr_let _
  | O.Expr.Expr_if_then_else _ | O.Expr.Expr_record _
  | O.Expr.Expr_record_update _ | O.Expr.Expr_apply _ | O.Expr.Expr_pattern _
  | O.Expr.Expr_accessor _ | O.Expr.Expr_access _ | O.Expr.Expr_record_extend _
  | O.Expr.Expr_record_select _ | O.Expr.Expr_record_empty | O.Expr.Expr_unit
  | O.Expr.Expr_kernel _ | O.Expr.Expr_lambda _ | O.Expr.Expr_list _
  | O.Expr.Expr_cons _ | O.Expr.Expr_tuple _ ->
      false

and shareable (clause_arr : O.Expr.expr_pattern_case array) (tree : DT.t) =
  match tree with
  | DT.Switch _ -> true
  | DT.Leaf { action; _ } -> not (trivial_action clause_arr.(action).expr)
  | DT.Fail -> false

and emit_match env (occ : J.expr) (clauses : O.Expr.expr_pattern_case list)
    ~terminating ~taken ~sink : J.stmt list =
  let tree, clause_arr = match_tree env clauses in
  let plan = DS.analyze ~shareable:(shareable clause_arr) tree in
  let clause_expr env action =
    emit_value env clause_arr.(action).O.Expr.expr
  in
  let leaf env action = taken env clause_arr.(action).O.Expr.expr in
  let tnames = Decisions.thunk_names env plan in
  Decisions.shared_thunks env occ ~plan ~tnames clause_expr
  @ Decisions.lower env occ ~terminating ~leaf ~fail:Decisions.match_failure ~sink ~plan ~tnames
      tree

and emit_match_return env tc (occ : J.expr)
    (clauses : O.Expr.expr_pattern_case list) : J.stmt list =
  emit_match env occ clauses ~terminating:true
    ~taken:(fun env chosen -> emit_return env tc chosen)
    ~sink:(fun e -> [ J.Return (Some e) ])

and emit_match_assign env (r : string) (occ : J.expr)
    (clauses : O.Expr.expr_pattern_case list) : J.stmt list =
  emit_match env occ clauses ~terminating:false
    ~taken:(fun env chosen ->
      let sa, ea = emit_value env chosen in
      sa @ [ J.assign r ea ])
    ~sink:(fun e -> [ J.assign r e ])

let decl_stmts env (decl : O.Declaration.t) : J.stmt list =
  let name = Names.of_loc decl.name in
  let decl = O.Declaration.merge_lambdas decl in
  match decl.params with
  | [] ->
      let s, e = emit_value env decl.body in
      s @ [ J.ConstDecl { name; init = e } ]
  | params ->
      let names =
        List.map
          (fun (p : O.Declaration.param) ->
            Data.Name.local (Data.Located.unwrap p.name))
          params
      in
      let env, param_names = Env.bind_params env names in
      let env =
        {
          env with
          Env.home =
            Some (Data.Name.local (Data.Located.unwrap decl.name), param_names);
        }
      in
      let tc =
        {
          fn = Data.Name.local (Data.Located.unwrap decl.name);
          js = name;
          params = param_names;
          triggered = false;
        }
      in
      let body = loop_body env tc decl.body in
      [ J.ConstDecl { name; init = J.arrow_of_body param_names body } ]

let is_defined_here (name : Data.Name.t) =
  match name with Data.Name.Local _ -> true | Data.Name.Global _ -> false

let constructor_decls names (constructors : (Data.Name.t * int) list) :
    J.stmt list =
  constructors
  |> List.filter (fun (name, _) ->
         is_defined_here name && not (Names.is_inline_constructor name))
  |> List.map (fun (name, arity) ->
         if arity = 0 then
           J.ConstDecl
             { name = Names.of_name name; init = Names.constructor_to_object names name [] }
         else
           let params = List.init arity Runtime.payload in
           let args = List.map (fun p -> J.Identifier p) params in
           J.ConstDecl
             {
               name = Names.of_name name;
               init =
                 J.Arrow
                   {
                     params;
                     body = J.ArrowExpr (Names.constructor_to_object names name args);
                   };
             })

let prepare ~blocks ~arities ~constructors ~siblings ~typedecls decls =
  let names = Names.create () in
  List.iter (fun (name, arity) -> Names.note_arity names name arity) arities;
  List.iter
    (fun (name, arity) ->
      if is_defined_here name then Names.reserve names (Names.of_name name);
      Names.note_arity names name arity)
    constructors;
  List.iter
    (fun (decl : O.Declaration.t) ->
      let src = Data.Name.local (Data.Located.unwrap decl.name) in
      Names.reserve names (Names.of_name src);
      Names.note_arity names src (O.Declaration.arity decl))
    decls;
  List.iter (fun (name, sibs) -> Names.note_siblings names name sibs) siblings;
  let forms = if blocks then Some (Forms.create ()) else None in
  { Env.scope = Scope.empty; names; instances = Instances.create typedecls; forms; home = None }

let program_with_helpers ~blocks ~arities ~constructors ~built ~siblings
    ~typedecls ~exports (decls : O.Declaration.t list) : J.program =
  let env = prepare ~blocks ~arities ~constructors ~siblings ~typedecls decls in
  let export_stmts =
    match exports with
    | [] -> []
    | names -> [ J.Export (List.map Names.of_name names) ]
  in
  let body = List.concat_map (decl_stmts env) decls in
  let forms = Option.fold ~none:[] ~some:Forms.declarations env.Env.forms in
  constructor_decls env.Env.names built
  @ Instances.declarations env.Env.instances
  @ forms @ body @ export_stmts

let import_lines imports =
  match imports with
  | [] -> ""
  | modules ->
      To_string.program_to_string
        (List.map
           (fun module_name ->
             J.Import_namespace
               {
                 local = module_ident module_name;
                 from = "./" ^ module_file module_name;
               })
           modules)

let emit_module ~blocks ~arities ~constructors ~built ~siblings
    ~typedecls ~imports ~exports (decls : O.Declaration.t list) :
    string * (string * string list) list =
  let program =
    program_with_helpers ~blocks ~arities ~constructors ~built ~siblings
      ~typedecls ~exports decls
  in
  let runtimes =
    List.filter_map
      (fun name ->
        match J.members_of ~object_:name program with
        | [] -> None
        | members -> Some (name, members))
      (runtime_module_name :: Platform_kernel.module_names)
  in
  ( import_lines (List.map fst runtimes @ imports)
    ^ To_string.program_to_string program,
    runtimes )
