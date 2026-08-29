module T = Typed
module O = Optimized
module Names = O.Expr.Names
module By_name = Data.Name.Map

let inline_size_limit = 10
let pattern_branch_cost = 2
let optimization_rounds = 3

let apply_to (fn : O.Expr.t) (arguments : O.Expr.t list) : O.Expr.t =
  List.fold_left
    (fun (fn : O.Expr.t) arg ->
      {
        O.Expr.typ = O.Type.result_after ~applied:1 fn.typ;
        expr = O.Expr.Expr_apply { fn; arg };
      })
    fn arguments

let boolean_constructor value : O.Expr.expr =
  O.Expr.Expr_constr { name = Primitives.bool_constructor value; arguments = [] }

let is_duplicable (e : O.Expr.t) : bool =
  match e.expr with
  | Expr_int _ | Expr_float _ | Expr_string _ | Expr_char _ | Expr_ident _ ->
      true
  | Expr_constr { arguments = []; _ } -> true
  | Expr_constr _ | Expr_binop _ | Expr_let _ | Expr_if_then_else _
  | Expr_record _ | Expr_record_update _ | Expr_apply _ | Expr_pattern _
  | Expr_accessor _ | Expr_access _ | Expr_record_extend _
  | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_kernel _
  | Expr_lambda _ | Expr_list _ | Expr_cons _ | Expr_tuple _ ->
      false

let rec is_pure (e : O.Expr.t) : bool =
  match e.expr with
  | Expr_lambda _ -> true
  | Expr_pattern _ -> false
  | Expr_apply _ -> begin
      match O.Expr.spine e with
      | { expr = Expr_ident name; _ }, ([ _; _ ] as operands)
        when Data.Operator.referred_to_by name <> None ->
          List.for_all is_pure operands
      | _ -> false
    end
  | Expr_constr _ | Expr_binop _ | Expr_let _ | Expr_if_then_else _
  | Expr_record _ | Expr_record_update _ | Expr_ident _ | Expr_accessor _
  | Expr_access _ | Expr_record_extend _ | Expr_record_select _
  | Expr_record_empty | Expr_unit | Expr_kernel _ | Expr_char _ | Expr_string _
  | Expr_int _ | Expr_float _ | Expr_list _ | Expr_cons _ | Expr_tuple _ ->
      List.for_all is_pure (O.Expr.children e)

let rec expression_size (e : O.Expr.t) : int =
  match e.expr with
  | Expr_pattern { expr; pattern_data_items } ->
      List.fold_left
        (fun acc (case : O.Expr.expr_pattern_case) ->
          acc + pattern_branch_cost + expression_size case.expr)
        (1 + expression_size expr)
        pattern_data_items
  | Expr_constr _ | Expr_binop _ | Expr_let _ | Expr_if_then_else _
  | Expr_record _ | Expr_record_update _ | Expr_apply _ | Expr_ident _
  | Expr_accessor _ | Expr_access _ | Expr_record_extend _
  | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_kernel _
  | Expr_lambda _ | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _
  | Expr_list _ | Expr_cons _ | Expr_tuple _ ->
      List.fold_left
        (fun acc child -> acc + expression_size child)
        1 (O.Expr.children e)

let occurs_free ~(name : Data.Name.t) (e : O.Expr.t) : bool =
  Names.mem name (O.Expr.free_variables ~bound:Names.empty e)

let rec zip_exactly parameters (arguments : O.Expr.t list) =
  match (parameters, arguments) with
  | [], [] -> Some []
  | parameter :: parameters, argument :: arguments ->
      zip_exactly parameters arguments
      |> Option.map (fun rest -> (parameter, argument) :: rest)
  | _ -> None

let bind_arguments (bindings : (string Data.Located.t * O.Expr.t) list)
    (body : O.Expr.t) : O.Expr.t =
  List.fold_right
    (fun (name, value) (inner : O.Expr.t) ->
      {
        O.Expr.typ = inner.typ;
        expr =
          O.Expr.Expr_let
            { binding = { bind_body = { name; body = value } }; body = inner };
      })
    bindings body

let arguments_escape_binders ~(binders : Names.t)
    ~(arguments : O.Expr.t list) : bool =
  List.exists
    (fun argument ->
      not
        (Names.disjoint binders
           (O.Expr.free_variables ~bound:Names.empty argument)))
    arguments

let fold_primitive (operator : Data.Operator.t) (arguments : O.Expr.t list) :
    O.Expr.expr option =
  let comparison order =
    match arguments with
    | [ { expr = Expr_int a; _ }; { expr = Expr_int b; _ } ] ->
        Some (boolean_constructor (order (Int.compare a b) 0))
    | _ -> None
  in
  let equality =
    match arguments with
    | [ { expr = Expr_int a; _ }; { expr = Expr_int b; _ } ] ->
        Some (boolean_constructor (a = b))
    | [ { expr = Expr_string a; _ }; { expr = Expr_string b; _ } ]
    | [ { expr = Expr_char a; _ }; { expr = Expr_char b; _ } ] ->
        Some (boolean_constructor (String.equal a b))
    | _ -> None
  in
  let integers combine : O.Expr.expr option =
    match arguments with
    | [ { expr = Expr_int a; _ }; { expr = Expr_int b; _ } ] ->
        Some (Expr_int (combine a b))
    | _ -> None
  in
  match operator with
  | Add -> integers ( + )
  | Subtract -> integers ( - )
  | Multiply -> integers ( * )
  | Divide -> begin
      match arguments with
      | [ { expr = Expr_int a; _ }; { expr = Expr_int b; _ } ]
        when b <> 0 && a mod b = 0 ->
          Some (Expr_int (a / b))
      | _ -> None
    end
  | Append -> begin
      match arguments with
      | [ { expr = Expr_string a; _ }; { expr = Expr_string b; _ } ] ->
          Some (Expr_string (a ^ b))
      | _ -> None
    end
  | Equal -> equality
  | Less -> comparison ( < )
  | Greater -> comparison ( > )
  | Integer_divide | Power | Not_equal | Less_or_equal | Greater_or_equal
  | Conjunction | Disjunction ->
      None

let propagate_constants (e : O.Expr.t) : O.Expr.t =
  let forget ~name constants =
    By_name.filter
      (fun bound value ->
        (not (Data.Name.equal bound name)) && not (occurs_free ~name value))
      constants
  in
  let forget_all names constants =
    Names.fold (fun name -> forget ~name) names constants
  in
  let rec propagate ~constants (e : O.Expr.t) : O.Expr.t =
    match e.expr with
    | Expr_ident name ->
        By_name.find_opt name constants
        |> Option.map (fun (value : O.Expr.t) -> { e with expr = value.expr })
        |> Option.value ~default:e
    | Expr_let { binding; body } ->
        let name = Data.Name.local (Data.Located.unwrap binding.bind_body.name) in
        let value = propagate ~constants binding.bind_body.body in
        let constants = forget ~name constants in
        let constants =
          if is_duplicable value then By_name.add name value constants
          else constants
        in
        {
          e with
          expr =
            Expr_let
              {
                binding =
                  { bind_body = { binding.bind_body with body = value } };
                body = propagate ~constants body;
              };
        }
    | Expr_lambda { params; body } ->
        let constants =
          forget_all (O.Expr.bound_by_lambda params) constants
        in
        {
          e with
          expr = Expr_lambda { params; body = propagate ~constants body };
        }
    | Expr_pattern { expr; pattern_data_items } ->
        let scrutinee = propagate ~constants expr in
        let propagate_case (case : O.Expr.expr_pattern_case) =
          let constants =
            forget_all (O.Pattern.bound case.pattern) constants
          in
          { case with expr = propagate ~constants case.expr }
        in
        {
          e with
          expr =
            Expr_pattern
              {
                expr = scrutinee;
                pattern_data_items = List.map propagate_case pattern_data_items;
              };
        }
    | Expr_if_then_else { if_exp; then_exp; else_exp } -> begin
        let if_exp = propagate ~constants if_exp in
        let then_exp = propagate ~constants then_exp in
        let else_exp = propagate ~constants else_exp in
        match if_exp.expr with
        | Expr_constr { name; arguments = [] }
          when Option.equal Bool.equal (Primitives.bool_of_constructor name) (Some true) ->
            then_exp
        | Expr_constr { name; arguments = [] }
          when Option.equal Bool.equal (Primitives.bool_of_constructor name) (Some false) ->
            else_exp
        | Expr_constr _ | Expr_binop _ | Expr_let _ | Expr_if_then_else _
        | Expr_record _ | Expr_record_update _ | Expr_apply _ | Expr_ident _
        | Expr_pattern _ | Expr_accessor _ | Expr_access _
        | Expr_record_extend _ | Expr_record_select _ | Expr_record_empty
        | Expr_unit | Expr_kernel _ | Expr_lambda _ | Expr_char _
        | Expr_string _ | Expr_int _ | Expr_float _ | Expr_list _ | Expr_cons _
        | Expr_tuple _ ->
            { e with expr = Expr_if_then_else { if_exp; then_exp; else_exp } }
      end
    | Expr_apply _ -> begin
        let inner = O.Expr.map_children e ~f:(propagate ~constants) in
        match O.Expr.spine inner with
        | { expr = Expr_ident name; _ }, ([ _; _ ] as arguments) ->
            Data.Operator.referred_to_by name
            |> Fun.flip Option.bind (fun operator ->
                   fold_primitive operator arguments)
            |> Option.map (fun expr -> { inner with expr })
            |> Option.value ~default:inner
        | _ -> inner
      end
    | Expr_constr _ | Expr_binop _ | Expr_record _ | Expr_record_update _
    | Expr_accessor _ | Expr_access _ | Expr_record_extend _
    | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_kernel _
    | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _ | Expr_list _
    | Expr_cons _ | Expr_tuple _ ->
        O.Expr.map_children e ~f:(propagate ~constants)
  in
  propagate ~constants:By_name.empty e

let rec reduce_beta (e : O.Expr.t) : O.Expr.t =
  let reduce (lambda : O.Expr.t) ~params ~body ~arguments =
    let taken = Int.min (List.length params) (List.length arguments) in
    let bound_params = List.take taken params in
    let bound_arguments = List.take taken arguments in
    let binders = O.Expr.bound_by_lambda bound_params in
    if arguments_escape_binders ~binders ~arguments:bound_arguments then None
    else
      let names =
        List.map (fun (p : O.Expr.expr_lambda_param) -> p.name) bound_params
      in
      let reduced_body = reduce_beta body in
      let applied_body =
        match List.drop taken params with
        | [] -> reduced_body
        | params ->
            {
              O.Expr.typ = O.Type.result_after ~applied:taken lambda.typ;
              expr = Expr_lambda { params; body = reduced_body };
            }
      in
      zip_exactly names bound_arguments
      |> Option.map (fun bindings ->
             apply_to
               (bind_arguments bindings applied_body)
               (List.drop taken arguments))
  in
  let reduction =
    match e.expr with
    | Expr_apply _ -> begin
        match O.Expr.spine e with
        | ( ({ expr = Expr_lambda { params; body }; _ } as lambda),
            (_ :: _ as arguments) ) ->
            let arguments = List.map reduce_beta arguments in
            reduce lambda ~params ~body ~arguments
        | { expr = Expr_let { binding; body }; _ }, (_ :: _ as arguments)
          when not
                 (arguments_escape_binders
                    ~binders:
                      (Names.singleton
                         (Data.Name.local
                            (Data.Located.unwrap binding.bind_body.name)))
                    ~arguments) ->
            Some
              {
                e with
                expr = Expr_let { binding; body = apply_to body arguments };
              }
        | _ -> None
      end
    | Expr_constr _ | Expr_binop _ | Expr_let _ | Expr_if_then_else _
    | Expr_record _ | Expr_record_update _ | Expr_ident _ | Expr_pattern _
    | Expr_accessor _ | Expr_access _ | Expr_record_extend _
    | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_kernel _
    | Expr_lambda _ | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _
    | Expr_list _ | Expr_cons _ | Expr_tuple _ ->
        None
  in
  match reduction with
  | Some result -> result
  | None -> O.Expr.map_children e ~f:reduce_beta

let rec eliminate_dead_lets (e : O.Expr.t) : O.Expr.t =
  let e = O.Expr.map_children e ~f:eliminate_dead_lets in
  match e.expr with
  | Expr_let { binding; body }
    when is_pure binding.bind_body.body
         && not
              (occurs_free
                 ~name:
                   (Data.Name.local (Data.Located.unwrap binding.bind_body.name))
                 body) ->
      body
  | Expr_constr _ | Expr_binop _ | Expr_let _ | Expr_if_then_else _
  | Expr_record _ | Expr_record_update _ | Expr_apply _ | Expr_ident _
  | Expr_pattern _ | Expr_accessor _ | Expr_access _ | Expr_record_extend _
  | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_kernel _
  | Expr_lambda _ | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _
  | Expr_list _ | Expr_cons _ | Expr_tuple _ ->
      e

type inlinable = {
  parameters : O.Declaration.param list;
  parameter_names : Names.t;
  body : O.Expr.t;
  free_names : Names.t;
}

let rec match_types bindings (parameter : O.Type.t) (argument : O.Type.t) =
  match (parameter, argument) with
  | TVar variable, _ ->
      if O.Type.By_variable.mem variable bindings then bindings
      else O.Type.By_variable.add variable argument bindings
  | TFun (from_parameter, to_parameter), TFun (from_argument, to_argument) ->
      match_types
        (match_types bindings from_parameter from_argument)
        to_parameter to_argument
  | TTup parameters, TTup arguments -> matched_all bindings parameters arguments
  | TCustom (_, parameters), TCustom (_, arguments) ->
      matched_all bindings parameters arguments
  | TRecord parameter_row, TRecord argument_row ->
      match_types bindings parameter_row argument_row
  | ( TRowExtend (_, parameter_field, parameter_rest),
      TRowExtend (_, argument_field, argument_rest) ) ->
      match_types
        (match_types bindings parameter_field argument_field)
        parameter_rest argument_rest
  | ( ( TInt | TFloat | TChar | TBool | TStr | TUnit | TRowEmpty | TFun _
      | TTup _ | TCustom _ | TRecord _ | TRowExtend _ ),
      _ ) ->
      bindings

and matched_all bindings parameters arguments =
  match (parameters, arguments) with
  | parameter :: parameters, argument :: arguments ->
      matched_all (match_types bindings parameter argument) parameters arguments
  | [], _ | _, [] -> bindings

let rec specialise bindings (e : O.Expr.t) : O.Expr.t =
  let e = O.Expr.map_children e ~f:(specialise bindings) in
  let expr =
    match e.expr with
    | Expr_lambda { params; body } ->
        O.Expr.Expr_lambda
          {
            params =
              List.map
                (fun (p : O.Expr.expr_lambda_param) ->
                  { p with typ = O.Type.substitute bindings p.typ })
                params;
            body;
          }
    | Expr_pattern { expr; pattern_data_items } ->
        O.Expr.Expr_pattern
          {
            expr;
            pattern_data_items =
              List.map
                (fun (case : O.Expr.expr_pattern_case) ->
                  {
                    case with
                    pattern = O.Pattern.substitute bindings case.pattern;
                  })
                pattern_data_items;
          }
    | untyped -> untyped
  in
  { typ = O.Type.substitute bindings e.typ; expr }

let inlinable_declarations (decls : O.Declaration.t list) :
    inlinable By_name.t =
  let candidate (d : O.Declaration.t) =
    let free_names = O.Declaration.free d in
    let is_inlinable =
      match d.params with
      | [] -> is_duplicable d.body
      | _ -> expression_size d.body <= inline_size_limit
    in
    if
      is_inlinable
      && not
           (Names.mem (Data.Name.local (Data.Located.unwrap d.name)) free_names)
    then
      Some
        {
          parameters = d.params;
          parameter_names = O.Declaration.bound d;
          body = d.body;
          free_names;
        }
    else None
  in
  List.fold_left
    (fun table (d : O.Declaration.t) ->
      match candidate d with
      | Some inlinable ->
          By_name.add
            (Data.Name.local (Data.Located.unwrap d.name))
            inlinable table
      | None -> table)
    By_name.empty decls

let rec inline_calls ~(table : inlinable By_name.t) ~(bound : Names.t)
    (e : O.Expr.t) : O.Expr.t =
  let inlined_call () =
    let head, arguments = O.Expr.spine e in
    let arguments = List.map (inline_calls ~table ~bound) arguments in
    let expand candidate =
      let arity = List.length candidate.parameters in
      let taken = List.take arity arguments in
      if
        Names.disjoint candidate.free_names bound
        && not
             (arguments_escape_binders ~binders:candidate.parameter_names
                ~arguments:taken)
      then
        zip_exactly candidate.parameters taken
        |> Option.map (fun bindings ->
               let specialisation =
                 List.fold_left
                   (fun collected
                        ((parameter : O.Declaration.param),
                         (argument : O.Expr.t)) ->
                     match_types collected parameter.typ argument.typ)
                   O.Type.By_variable.empty bindings
               in
               let bound =
                 List.map
                   (fun ((parameter : O.Declaration.param), argument) ->
                     (parameter.name, argument))
                   bindings
               in
               apply_to
                 (bind_arguments bound
                    (specialise specialisation candidate.body))
                 (List.drop arity arguments))
      else None
    in
    let unbound name =
      if Names.mem name bound then None
      else Option.bind (By_name.find_opt name table) expand
    in
    Option.bind (O.Expr.ident_of head) unbound
  in
  match e.expr with
  | Expr_ident _ | Expr_apply _ -> begin
      match inlined_call () with
      | Some result -> result
      | None -> O.Expr.map_children e ~f:(inline_calls ~table ~bound)
    end
  | Expr_let { binding; body } ->
      let name = Data.Name.local (Data.Located.unwrap binding.bind_body.name) in
      let value = inline_calls ~table ~bound binding.bind_body.body in
      {
        e with
        expr =
          Expr_let
            {
              binding = { bind_body = { binding.bind_body with body = value } };
              body = inline_calls ~table ~bound:(Names.add name bound) body;
            };
      }
  | Expr_lambda { params; body } ->
      let bound = Names.union bound (O.Expr.bound_by_lambda params) in
      {
        e with
        expr = Expr_lambda { params; body = inline_calls ~table ~bound body };
      }
  | Expr_pattern { expr; pattern_data_items } ->
      let scrutinee = inline_calls ~table ~bound expr in
      let inline_case (case : O.Expr.expr_pattern_case) =
        let bound =
          Names.union bound (O.Pattern.bound case.pattern)
        in
        { case with expr = inline_calls ~table ~bound case.expr }
      in
      {
        e with
        expr =
          Expr_pattern
            {
              expr = scrutinee;
              pattern_data_items = List.map inline_case pattern_data_items;
            };
      }
  | Expr_constr _ | Expr_binop _ | Expr_if_then_else _ | Expr_record _
  | Expr_record_update _ | Expr_accessor _ | Expr_access _
  | Expr_record_extend _ | Expr_record_select _ | Expr_record_empty | Expr_unit
  | Expr_kernel _ | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _
  | Expr_list _ | Expr_cons _ | Expr_tuple _ ->
      O.Expr.map_children e ~f:(inline_calls ~table ~bound)

let optimize (decls : T.Declaration.t list) : O.Declaration.t list =
  let round decls =
    let table = inlinable_declarations decls in
    List.map
      (fun (d : O.Declaration.t) ->
        let bound = O.Declaration.bound d in
        let body =
          d.body
          |> inline_calls ~table ~bound
          |> reduce_beta
          |> propagate_constants
          |> eliminate_dead_lets
        in
        { d with body })
      decls
  in
  let rec rounds ~remaining decls =
    if remaining <= 0 then decls
    else rounds ~remaining:(remaining - 1) (round decls)
  in
  List.map O.Declaration.of_typed decls |> List.map O.Declaration.saturate
  |> rounds ~remaining:optimization_rounds
