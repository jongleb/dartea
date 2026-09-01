module J = Ast
module O = Optimized
module Scope = Data.Name.Map

type arity = O.Type.arity = Exactly of int | At_least of int

let needs_temp_var = function
  | J.Identifier _ | J.Literal _ -> false
  | J.Binary _ | J.Unary _ | J.Call _ | J.New _ | J.Function _ | J.Arrow _
  | J.Member _ | J.Conditional _ | J.Object _ | J.Array _ | J.Assignment _ ->
      true

let shared_operand env expression =
  if needs_temp_var expression then
    let name = Env.temp env in
    ([ J.ConstDecl { name; init = expression } ], J.Identifier name)
  else ([], expression)

let shared_operands env ~when_duplicated left right =
  if when_duplicated then
    let left_bindings, left = shared_operand env left in
    let right_bindings, right = shared_operand env right in
    (left_bindings @ right_bindings, left, right)
  else ([], left, right)

let lower_binary env ~(operand : O.Type.t) (operator : Data.Operator.t) left
    right : J.stmt list * J.expr =
  let bindings, left, right =
    shared_operands env
      ~when_duplicated:
        (Instances.reads_operands_twice env.Env.instances operand operator)
      left right
  in
  (bindings, Instances.lower env.Env.instances ~operand operator left right)

let lower_method env (method_ : Data.Method.t) ~operand left right =
  let bindings, left, right =
    shared_operands env ~when_duplicated:true left right
  in
  let pick test =
    J.Conditional { test; consequent = left; alternate = right }
  in
  let extreme operator =
    Instances.ordering_of env.Env.instances ~budget:Instances.expansion_budget
      ~operator operand left right
  in
  match method_ with
  | Minimum -> (bindings, pick (extreme J.LessThan))
  | Maximum -> (bindings, pick (extreme J.GreaterThan))
  | Compare ->
      let name = Env.temp env in
      let sign = J.Identifier name in
      let result = Data.Method.ordering_result in
      let nullary ctor = Names.constructor_to_object env.Env.names ctor [] in
      ( bindings
        @ [
            J.ConstDecl
              {
                name;
                init = Instances.three_way_of env.Env.instances operand left right;
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

let lower_full_call env name ~operand =
  if Scope.mem name env.Env.scope then None
  else
    match Data.Operator.referred_to_by name with
    | Some operator -> Some (lower_binary env ~operand operator)
    | None ->
        Option.map
          (fun method_ -> lower_method env method_ ~operand)
          (Data.Method.referred_to_by name)


let curry_call f args =
  let count = List.length args in
  if count >= 1 && count <= Runtime.widest_apply then
    J.call (Names.apply_reference count) (f :: args)
  else J.call Names.curry_reference [ f; J.Array args ]

let split_at n lst =
  let rec go i acc = function
    | rest when i = 0 -> (List.rev acc, rest)
    | x :: rest -> go (i - 1) (x :: acc) rest
    | [] -> (List.rev acc, [])
  in
  go n [] lst


let arity_of_type = O.Type.arity

let closure_partial env callee args missing =
  let rparams = List.init missing (fun _ -> Env.temp env) in
  let rargs = List.map (fun p -> J.Identifier p) rparams in
  J.Arrow
    { params = rparams; body = J.ArrowExpr (J.call callee (args @ rargs)) }
