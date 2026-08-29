module J = Ast
module O = Optimized
module DT = After_typed.Exhaustive.Decision_tree
module Occ = After_typed.Exhaustive.Occurrence
module DS = After_typed.Exhaustive.Share
module Scope = Data.Name.Map

type env = { scope : string Scope.t; names : Names.t; instances : Instances.t }

let runtime_module_name = Names.runtime_module
let module_ident = Names.module_ident
let js_of_name = Names.of_name
let extension = "mjs"
let module_suffix = "." ^ extension
let module_file module_name = module_name ^ module_suffix
let temp env = Names.temp env.names

let bind_one env src =
  let js = Names.fresh env.names (Names.of_name src) in
  ({ env with scope = Scope.add src js env.scope }, js)

let jid_env env src =
  match Scope.find_opt src env.scope with
  | Some js -> J.Identifier js
  | None -> Names.expression_of src

let is_bool_constructor name = Option.is_some (Primitives.bool_of_constructor name)
let is_unit_constructor = Primitives.is_unit_constructor

let bool_literal name =
  J.bool (Option.equal Bool.equal (Primitives.bool_of_constructor name) (Some true))

let is_inline_constructor name =
  is_bool_constructor name || is_unit_constructor name

let payload_fields js_arguments =
  List.mapi (fun i a -> J.Field (Runtime.payload i, a)) js_arguments

let constructor_to_object names name js_arguments =
  if is_bool_constructor name then bool_literal name
  else if is_unit_constructor name then J.Literal J.Null
  else if List.is_empty js_arguments then J.string (Data.Name.base name)
  else if Names.is_tag_omitted names name then
    J.Object (payload_fields js_arguments)
  else
    J.Object
      (J.Field (Runtime.tag, J.string (Data.Name.base name))
      :: payload_fields js_arguments)

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

let needs_temp_var = function
  | J.Identifier _ | J.Literal _ -> false
  | J.Binary _ | J.Unary _ | J.Call _ | J.New _ | J.Function _ | J.Arrow _
  | J.Member _ | J.Conditional _ | J.Object _ | J.Array _ | J.Assignment _ ->
      true

let shared_operand env expression =
  if needs_temp_var expression then
    let name = temp env in
    ([ J.ConstDecl { name; init = expression } ], J.Identifier name)
  else ([], expression)

let shared_operands env ~when_duplicated left right =
  if when_duplicated then
    let left_bindings, left = shared_operand env left in
    let right_bindings, right = shared_operand env right in
    (left_bindings @ right_bindings, left, right)
  else ([], left, right)

let binary_lowering env ~(operand : O.Type.t) (operator : Data.Operator.t) left
    right : J.stmt list * J.expr =
  let bindings, left, right =
    shared_operands env
      ~when_duplicated:
        (Instances.reads_operands_twice env.instances operand operator)
      left right
  in
  (bindings, Instances.lower env.instances ~operand operator left right)

let method_lowering env (method_ : Data.Method.t) ~operand left right =
  let bindings, left, right =
    shared_operands env ~when_duplicated:true left right
  in
  let pick test =
    J.Conditional { test; consequent = left; alternate = right }
  in
  let extreme operator =
    Instances.ordering_of env.instances ~budget:Instances.expansion_budget
      ~operator operand left right
  in
  match method_ with
  | Minimum -> (bindings, pick (extreme J.LessThan))
  | Maximum -> (bindings, pick (extreme J.GreaterThan))
  | Compare ->
      let name = temp env in
      let sign = J.Identifier name in
      let result = Data.Method.ordering_result in
      let nullary ctor = constructor_to_object env.names ctor [] in
      ( bindings
        @ [
            J.ConstDecl
              {
                name;
                init = Instances.three_way_of env.instances operand left right;
              };
          ],
        J.Conditional
          {
            test = J.binary J.LessThan sign (J.int 0);
            consequent = nullary result.less;
            alternate =
              J.Conditional
                {
                  test = J.binary J.StrictEqual sign (J.int 0);
                  consequent = nullary result.equal;
                  alternate = nullary result.greater;
                };
          } )

let saturated_lowering env name ~operand =
  if Scope.mem name env.scope then None
  else
    match Data.Operator.referred_to_by name with
    | Some operator -> Some (binary_lowering env ~operand operator)
    | None ->
        Option.map
          (fun method_ -> method_lowering env method_ ~operand)
          (Data.Method.referred_to_by name)

let occ_expr root (o : Occ.t) : J.expr =
  List.fold_left
    (fun e step ->
      match step with
      | Occ.Payload i -> J.member e (Runtime.payload i)
      | Occ.Index i -> J.at_index e i
      | Occ.Field f -> J.member e f
      | Occ.Hd -> J.member e Runtime.head
      | Occ.Tl -> J.member e Runtime.tail)
    root o

let ctor_literal name =
  match Primitives.bool_of_constructor name with
  | Some truth -> J.Bool truth
  | None -> J.String (Data.Name.base name)

let js_eq left literal = J.binary J.StrictEqual left (J.Literal literal)

let test_expr env occ_e (test : DT.test) : J.expr =
  match test with
  | DT.Test_ctor name -> js_eq occ_e (ctor_literal name)
  | DT.Test_tag name ->
      if Names.is_tag_omitted env.names name then J.is_object occ_e
      else js_eq (J.member occ_e Runtime.tag) (J.String (Data.Name.base name))
  | DT.Test_int n -> js_eq occ_e (J.Int n)
  | DT.Test_str s -> js_eq occ_e (J.String s)
  | DT.Test_chr c -> js_eq occ_e (J.String c)
  | DT.Test_nil -> js_eq occ_e (J.Int 0)
  | DT.Test_cons -> J.binary J.StrictNotEqual occ_e (J.int 0)

type discriminant = By_tag | By_value

let same_discriminant one other =
  match (one, other) with
  | By_tag, By_tag | By_value, By_value -> true
  | By_tag, By_value | By_value, By_tag -> false

let switch_key env (test : DT.test) : (discriminant * J.literal) option =
  match test with
  | DT.Test_tag n when not (Names.is_tag_omitted env.names n) ->
      Some (By_tag, J.String (Data.Name.base n))
  | DT.Test_tag _ -> None
  | DT.Test_ctor n when not (is_bool_constructor n) ->
      Some (By_value, J.String (Data.Name.base n))
  | DT.Test_int n -> Some (By_value, J.Int n)
  | DT.Test_str s -> Some (By_value, J.String s)
  | DT.Test_chr c -> Some (By_value, J.String c)
  | DT.Test_ctor _ | DT.Test_nil | DT.Test_cons -> None

let switch_plan env occ_e (branches : (DT.test * DT.t) list) :
    (J.expr * (J.literal * DT.t) list) option =
  let keys =
    List.fold_right
      (fun (test, subtree) collected ->
        match (collected, switch_key env test) with
        | Some cases, Some (kind, literal) ->
            Some ((kind, literal, subtree) :: cases)
        | _ -> None)
      branches (Some [])
  in
  match keys with
  | None | Some [] -> None
  | Some ((kind, _, _) :: _ as cases) ->
      if List.for_all (fun (other, _, _) -> same_discriminant other kind) cases
      then
        let discriminant =
          match kind with By_tag -> J.member occ_e Runtime.tag | By_value -> occ_e
        in
        Some
          ( discriminant,
            List.map (fun (_, literal, subtree) -> (literal, subtree)) cases )
      else None

let match_failure =
  [
    J.Throw
      (J.New
         {
           callee = J.Identifier "Error";
           args = [ J.string "Pattern match failed" ];
         });
  ]

let curry_call f args =
  J.call Names.curry_reference [ f; J.Array args ]

let split_at n lst =
  let rec go i acc = function
    | rest when i = 0 -> (List.rev acc, rest)
    | x :: rest -> go (i - 1) (x :: acc) rest
    | [] -> (List.rev acc, [])
  in
  go n [] lst

let declared_arity env name =
  if Scope.mem name env.scope then None else Names.arity_of env.names name

type arity = O.Type.arity = Exactly of int | At_least of int

let arity_of_type = O.Type.arity

let closure_partial env callee args missing =
  let rparams = List.init missing (fun _ -> temp env) in
  let rargs = List.map (fun p -> J.Identifier p) rparams in
  J.Arrow
    { params = rparams; body = J.ArrowExpr (J.call callee (args @ rargs)) }

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
  params : string list;
  mutable triggered : bool;
}

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
      body = J.ArrowExpr (J.member (J.Identifier "r") (Data.Located.unwrap field));
    }

let arrow_of_body params stmts =
  let body =
    match stmts with
    | [ J.Return (Some e) ] -> J.ArrowExpr e
    | _ -> J.ArrowBlock stmts
  in
  J.Arrow { params; body }

let thunk_names env plan =
  List.map
    (fun (id, _) -> (id, Names.fresh env.names ("$dt" ^ string_of_int id)))
    (DS.nodes plan)

let rec lower env root ~terminating ~leaf ~fail ~sink ~plan ~tnames
    (tree : DT.t) : J.stmt list =
  match DS.id_of plan tree with
  | Some id -> sink (J.call (J.Identifier (List.assoc id tnames)) [])
  | None -> lower_node env root ~terminating ~leaf ~fail ~sink ~plan ~tnames tree

and lower_node env root ~terminating ~leaf ~fail ~sink ~plan ~tnames
    (tree : DT.t) : J.stmt list =
  match tree with
  | DT.Fail -> fail
  | DT.Leaf { action; bindings } ->
      let jbinds =
        List.map (fun (v, o) -> (Data.Name.local v, occ_expr root o)) bindings
      in
      let env', bstmts = bind_binds env jbinds in
      bstmts @ leaf env' action
  | DT.Switch { occurrence; branches; default } -> begin
      let occ_e = occ_expr root occurrence in
      let go tr =
        lower env root ~terminating ~leaf ~fail ~sink ~plan ~tnames tr
      in
      let many_cases (disc, cases) =
        if List.length cases >= 2 then Some (disc, cases) else None
      in
      let switch_of =
        if terminating then switch_plan env occ_e branches else None
      in
      match Option.bind switch_of many_cases with
      | Some (disc, cases) ->
          let default_case =
            default
            |> Option.map (fun t -> { J.test = None; consequent = go t })
            |> Option.to_list
          in
          let js_cases =
            List.map
              (fun (lit, tr) ->
                { J.test = Some (J.Literal lit); consequent = go tr })
              cases
          in
          [ J.Switch { discriminant = disc; cases = js_cases @ default_case } ]
      | None ->
          let rec build = function
            | [] -> Option.fold ~none:fail ~some:go default
            | [ (_, tr) ] when Option.is_none default -> go tr
            | (test, tr) :: rest ->
                [
                  J.If
                    {
                      test = test_expr env occ_e test;
                      consequent = go tr;
                      alternate = Some (build rest);
                    };
                ]
          in
          build branches
    end

let shared_thunks env root ~plan ~tnames clause_expr =
  let sink e = [ J.Return (Some e) ] in
  let leaf env action =
    let sa, ea = clause_expr env action in
    sa @ [ J.Return (Some ea) ]
  in
  List.map
    (fun (id, sub) ->
      let body =
        lower_node env root ~terminating:true ~leaf ~fail:match_failure ~sink
          ~plan ~tnames sub
      in
      J.ConstDecl { name = List.assoc id tnames; init = arrow_of_body [] body })
    (DS.nodes plan)

let rec emit_value env (e : O.Expr.t) : J.stmt list * J.expr =
  let statements, expression = emit_uncoerced env e in
  ( statements,
    coerce env expression
      ~expected:(arity_of_type e.O.Expr.typ)
      ~actual:(emitted_arity env e) )

and coerce env expression ~expected ~actual =
  match (expected, actual) with
  | Exactly wanted, Exactly given
    when wanted <> given && wanted >= 1 && given >= 1 ->
      let params = List.init wanted (fun _ -> temp env) in
      let arguments = List.map (fun p -> J.Identifier p) params in
      let call_in_two_steps () =
        let first, extra = split_at given arguments in
        J.call (J.call expression first) extra
      in
      let body =
        if given < wanted then call_in_two_steps ()
        else closure_partial env expression arguments (given - wanted)
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
        match declared_arity env name with
        | Some n when n > given_count -> Exactly (n - given_count)
        | Some _ | None -> arity_of_type e.typ
      end
  | O.Expr.Expr_ident _ -> callee_arity env e
  | O.Expr.Expr_apply { fn; _ } when is_record_construction fn -> Exactly 0
  | O.Expr.Expr_apply { fn; arg } ->
      let callee, args = applied_spine ~fn ~arg in
      let given_count = List.length args in
      begin
        match callee_arity env callee with
        | Exactly n when n > given_count -> Exactly (n - given_count)
        | Exactly n when n >= 1 -> arity_of_type e.typ
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
      arity_of_type e.typ

and callee_arity env (callee : O.Expr.t) : arity =
  Option.bind (O.Expr.ident_of callee) (declared_arity env)
  |> Option.fold ~none:(arity_of_type callee.typ) ~some:(fun n -> Exactly n)

and emit_uncoerced env (e : O.Expr.t) : J.stmt list * J.expr =
  match e.expr with
  | O.Expr.Expr_int n -> ([], J.int n)
  | O.Expr.Expr_float f -> ([], J.Literal (J.Float f))
  | O.Expr.Expr_string s -> ([], J.string s)
  | O.Expr.Expr_char c -> ([], J.string c)
  | O.Expr.Expr_ident name when is_inline_constructor name ->
      ([], constructor_to_object env.names name [])
  | O.Expr.Expr_ident name -> ([], jid_env env name)
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
  | O.Expr.Expr_record_extend name -> ([], jid_env env (Data.Name.local name))
  | O.Expr.Expr_record_select name -> ([], jid_env env (Data.Name.local name))
  | O.Expr.Expr_accessor field -> ([], accessor_arrow field)
  | O.Expr.Expr_access { expr; field } ->
      let s, o = emit_value env expr in
      (s, J.member o (Data.Located.unwrap field))
  | O.Expr.Expr_binop { name; operands = a, b } ->
      let sa, ea = emit_value env a in
      let sb, eb = emit_value env b in
      let bindings, lowered =
        binary_lowering env ~operand:a.O.Expr.typ name ea eb
      in
      (sa @ sb @ bindings, lowered)
  | O.Expr.Expr_constr { name; arguments } ->
      let ss, es = emit_values env arguments in
      (ss, constructor_to_object env.names name es)
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
      let sr, er = emit_value env record in
      let ss, members = emit_fields env fields in
      (sr @ ss, J.Object (J.Spread er :: members))
  | O.Expr.Expr_apply { fn; arg } -> emit_apply env fn arg
  | O.Expr.Expr_lambda { params; body } -> ([], emit_lambda env params body)
  | O.Expr.Expr_if_then_else { if_exp; then_exp; else_exp } ->
      let sc, ec = emit_value env if_exp in
      let st, et = emit_value env then_exp in
      let se, ee = emit_value env else_exp in
      if List.is_empty st && List.is_empty se then
        (sc, J.Conditional { test = ec; consequent = et; alternate = ee })
      else
        let r = temp env in
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
      let r = temp env in
      let chain = emit_match_assign env r occ pattern_data_items in
      ( ss @ sbind @ [ J.VarDecl { name = r; init = None } ] @ chain,
        J.Identifier r )

and emit_values env (es : O.Expr.t list) : J.stmt list * J.expr list =
  fold_emit (emit_value env) es

and emit_fields env (rows : O.Expr.expr_record_row list) :
    J.stmt list * J.object_member list =
  fold_emit
    (fun { O.Expr.name; value } ->
      let s, v = emit_value env value in
      (s, J.Field (name, v)))
    rows

and emit_binding env (binding : O.Expr.expr_let_binding) =
  let env', name =
    bind_one env (Data.Name.local (Data.Located.unwrap binding.bind_body.name))
  in
  let sv, ev = emit_value env' binding.bind_body.body in
  (env', sv @ [ J.ConstDecl { name; init = ev } ])

and emit_scrutinee env (expr : O.Expr.t) : J.stmt list * J.expr * J.stmt list =
  let s, e = emit_value env expr in
  if needs_temp_var e then
    let t = temp env in
    (s, J.Identifier t, [ J.ConstDecl { name = t; init = e } ])
  else (s, e, [])

and emit_apply env fn arg =
  if is_record_construction fn then emit_record_apply env fn arg
  else
    let callee, args = applied_spine ~fn ~arg in
    let saturated_operator =
      match args with
      | [ left; right ] ->
          let lower_op op =
            saturated_lowering env op ~operand:left.O.Expr.typ
          in
          Option.bind (O.Expr.ident_of callee) lower_op
          |> Option.map (fun lower -> (lower, left, right))
      | _ -> None
    in
    match saturated_operator with
    | Some (lower, left, right) ->
        let sa, ea = emit_value env left in
        let sb, eb = emit_value env right in
        let bindings, lowered = lower ea eb in
        (sa @ sb @ bindings, lowered)
    | None -> begin
        match (callee.expr, args) with
        | O.Expr.Expr_kernel (Kernel_value kernel), _ ->
            let arity = Data.Kernel.arity kernel in
            emit_known_call env (Of_kernel.value kernel) ~arity
              ~result_type:
                (O.Type.result_after ~applied:arity callee.O.Expr.typ)
              args
        | O.Expr.Expr_ident name, _ -> begin
            match declared_arity env name with
            | Some n when n >= 1 ->
                emit_known_call env (jid_env env name) ~arity:n
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
      end

and emit_known_call env callee ~arity ~result_type args =
  let statements, arguments = emit_values env args in
  apply env callee ~arity ~result_type ~statements ~arguments

and apply env callee ~arity ~result_type ~statements ~arguments =
  let given_count = List.length arguments in
  if given_count = arity then (statements, J.call callee arguments)
  else if given_count < arity then
    (statements, closure_partial env callee arguments (arity - given_count))
  else
    let first, extra = split_at arity arguments in
    let head_call = J.call callee first in
    match arity_of_type result_type with
    | Exactly n when n >= 1 ->
        apply env head_call ~arity:n
          ~result_type:(O.Type.result_after ~applied:n result_type)
          ~statements ~arguments:extra
    | Exactly _ | At_least _ -> (statements, curry_call head_call extra)

and emit_generic env callee args =
  let sc, ec = emit_value env callee in
  let ss, es = emit_values env args in
  match arity_of_type callee.O.Expr.typ with
  | Exactly arity when arity >= 1 ->
      let bound, target =
        if List.length es < arity && needs_temp_var ec then
          let t = temp env in
          ([ J.ConstDecl { name = t; init = ec } ], J.Identifier t)
        else ([], ec)
      in
      let statements, expression =
        apply env target ~arity
          ~result_type:(O.Type.result_after ~applied:arity callee.O.Expr.typ)
          ~statements:ss ~arguments:es
      in
      (sc @ bound @ statements, expression)
  | Exactly _ | At_least _ -> (sc @ ss, curry_call ec es)

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

and emit_lambda env params body =
  let names =
    List.map
      (fun (p : O.Expr.expr_lambda_param) ->
        Data.Name.local (Data.Located.unwrap p.name))
      params
  in
  let env, param_names = bind_params env names in
  arrow_of_body param_names (emit_return env None body)

and self_tail_args env tc (e : O.Expr.t) : O.Expr.t list option =
  let callee, args = O.Expr.spine e in
  let self_call name =
    if
      Data.Name.equal name tc.fn
      && (not (Scope.mem name env.scope))
      && List.length args = List.length tc.params
    then Some args
    else None
  in
  match args with
  | [] -> None
  | _ :: _ -> Option.bind (O.Expr.ident_of callee) self_call

and loop_step env tc args =
  let ss, es = emit_values env args in
  let temps = List.map (fun _ -> temp env) es in
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
  ( After_typed.Exhaustive.build (Names.siblings_of env.names) patterns,
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
  let tnames = thunk_names env plan in
  shared_thunks env occ ~plan ~tnames clause_expr
  @ lower env occ ~terminating ~leaf ~fail:match_failure ~sink ~plan ~tnames
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
      let env, param_names = bind_params env names in
      let tc =
        {
          fn = Data.Name.local (Data.Located.unwrap decl.name);
          params = param_names;
          triggered = false;
        }
      in
      let body = emit_return env (Some tc) decl.body in
      let body =
        if tc.triggered then [ J.While { test = J.bool true; body } ] else body
      in
      [ J.ConstDecl { name; init = arrow_of_body param_names body } ]

let is_defined_here (name : Data.Name.t) =
  match name with Data.Name.Local _ -> true | Data.Name.Global _ -> false

let constructor_decls names (constructors : (Data.Name.t * int) list) :
    J.stmt list =
  constructors
  |> List.filter (fun (name, _) ->
         is_defined_here name && not (is_inline_constructor name))
  |> List.map (fun (name, arity) ->
         if arity = 0 then
           J.ConstDecl
             { name = Names.of_name name; init = constructor_to_object names name [] }
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
                     body = J.ArrowExpr (constructor_to_object names name args);
                   };
             })

let prepare ~arities ~constructors ~siblings ~typedecls decls =
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
  { scope = Scope.empty; names; instances = Instances.create typedecls }

let program_with_helpers ~arities ~constructors ~built ~siblings ~typedecls
    ~exports (decls : O.Declaration.t list) : J.program =
  let env = prepare ~arities ~constructors ~siblings ~typedecls decls in
  let export_stmts =
    match exports with
    | [] -> []
    | names -> [ J.Export (List.map Names.of_name names) ]
  in
  let body = List.concat_map (decl_stmts env) decls in
  constructor_decls env.names built
  @ Instances.declarations env.instances
  @ body @ export_stmts

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

let comment_lines lines =
  match lines with
  | [] -> ""
  | spoken ->
      To_string.program_to_string (List.map (fun line -> J.Comment line) spoken)

let emit_module ~notice ~arities ~constructors ~built ~siblings ~typedecls
    ~imports ~exports (decls : O.Declaration.t list) :
    string * (string * string list) list =
  let program =
    program_with_helpers ~arities ~constructors ~built ~siblings ~typedecls
      ~exports decls
  in
  let runtimes =
    List.filter_map
      (fun name ->
        match J.members_of ~object_:name program with
        | [] -> None
        | members -> Some (name, members))
      (runtime_module_name :: Platform_kernel.module_names)
  in
  ( comment_lines notice
    ^ import_lines (List.map fst runtimes @ imports)
    ^ To_string.program_to_string program,
    runtimes )
