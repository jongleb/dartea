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
  | Expr_kernel (Kernel_value (Platform _)) -> true
  | Expr_constr _ | Expr_binop _ | Expr_let _ | Expr_if_then_else _
  | Expr_record _ | Expr_record_update _ | Expr_apply _ | Expr_pattern _
  | Expr_accessor _ | Expr_access _ | Expr_record_extend _
  | Expr_record_select _ | Expr_record_empty | Expr_unit
  | Expr_kernel (Kernel_value (Language _) | Kernel_unary _ | Kernel_binary _)
  | Expr_lambda _ | Expr_list _ | Expr_cons _ | Expr_tuple _ ->
      false

let rec is_pure (e : O.Expr.t) : bool =
  match e.expr with
  | Expr_lambda _ -> true
  | Expr_pattern _ -> false
  | Expr_apply _ -> begin
      match O.Expr.spine e with
      | { expr = Expr_ident name; _ }, ([ _; _ ] as operands)
        when Option.is_some (Data.Operator.referred_to_by name) ->
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

type use = { count : int; under_lambda : bool }

let rec uses ~(name : Data.Name.t) ~under_lambda (e : O.Expr.t) : use =
  let sum found (child : O.Expr.t) =
    let inner = uses ~name ~under_lambda child in
    {
      count = found.count + inner.count;
      under_lambda = found.under_lambda || inner.under_lambda;
    }
  in
  let none = { count = 0; under_lambda = false } in
  match e.expr with
  | Expr_ident found ->
      if Data.Name.equal found name then { count = 1; under_lambda } else none
  | Expr_let { binding; body } ->
      let bound = Data.Name.local (Data.Located.unwrap binding.bind_body.name) in
      let value = uses ~name ~under_lambda binding.bind_body.body in
      if Data.Name.equal bound name then value else sum value body
  | Expr_lambda { params; body } ->
      if Names.mem name (O.Expr.bound_by_lambda params) then none
      else uses ~name ~under_lambda:true body
  | Expr_pattern { expr; pattern_data_items } ->
      List.fold_left
        (fun found (case : O.Expr.expr_pattern_case) ->
          if Names.mem name (O.Pattern.bound case.pattern) then found
          else sum found case.expr)
        (uses ~name ~under_lambda expr)
        pattern_data_items
  | Expr_constr _ | Expr_binop _ | Expr_if_then_else _ | Expr_record _
  | Expr_record_update _ | Expr_apply _ | Expr_accessor _ | Expr_access _
  | Expr_record_extend _ | Expr_record_select _ | Expr_record_empty | Expr_unit
  | Expr_kernel _ | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _
  | Expr_list _ | Expr_cons _ | Expr_tuple _ ->
      List.fold_left sum none (O.Expr.children e)

exception Capture

let rec replace ~(name : Data.Name.t) ~(value : O.Expr.t) ~(outer : Names.t)
    (e : O.Expr.t) : O.Expr.t =
  let inside = replace ~name ~value ~outer in
  let guard binders =
    if Names.disjoint binders outer then () else raise Capture
  in
  match e.expr with
  | Expr_ident found ->
      if Data.Name.equal found name then { value with typ = e.typ } else e
  | Expr_let { binding; body } ->
      let bound = Data.Name.local (Data.Located.unwrap binding.bind_body.name) in
      let bound_value = inside binding.bind_body.body in
      let rest =
        if Data.Name.equal bound name then body
        else begin
          guard (Names.singleton bound);
          inside body
        end
      in
      {
        e with
        expr =
          Expr_let
            {
              binding = { bind_body = { binding.bind_body with body = bound_value } };
              body = rest;
            };
      }
  | Expr_lambda { params; body } ->
      let binders = O.Expr.bound_by_lambda params in
      if Names.mem name binders then e
      else begin
        guard binders;
        { e with expr = Expr_lambda { params; body = inside body } }
      end
  | Expr_pattern { expr; pattern_data_items } ->
      let scrutinee = inside expr in
      let inside_case (case : O.Expr.expr_pattern_case) =
        let binders = O.Pattern.bound case.pattern in
        if Names.mem name binders then case
        else begin
          guard binders;
          { case with expr = inside case.expr }
        end
      in
      {
        e with
        expr =
          Expr_pattern
            {
              expr = scrutinee;
              pattern_data_items = List.map inside_case pattern_data_items;
            };
      }
  | Expr_constr _ | Expr_binop _ | Expr_if_then_else _ | Expr_record _
  | Expr_record_update _ | Expr_apply _ | Expr_accessor _ | Expr_access _
  | Expr_record_extend _ | Expr_record_select _ | Expr_record_empty | Expr_unit
  | Expr_kernel _ | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _
  | Expr_list _ | Expr_cons _ | Expr_tuple _ ->
      O.Expr.map_children e ~f:inside

let substitute ~(name : Data.Name.t) ~(value : O.Expr.t) (body : O.Expr.t) :
    O.Expr.t option =
  let use = uses ~name ~under_lambda:false body in
  let is_linear = use.count = 1 && not use.under_lambda in
  if use.count = 0 || is_linear || is_duplicable value then
    let outer = O.Expr.free_variables ~bound:Names.empty value in
    match replace ~name ~value ~outer body with
    | body -> Some body
    | exception Capture -> None
  else None

let bind_arguments (bindings : (string Data.Located.t * O.Expr.t) list)
    (body : O.Expr.t) : O.Expr.t =
  List.fold_right
    (fun (name, value) (inner : O.Expr.t) ->
      let bound =
        {
          O.Expr.typ = inner.typ;
          expr =
            O.Expr.Expr_let
              { binding = { bind_body = { name; body = value } }; body = inner };
        }
      in
      substitute ~name:(Data.Name.local (Data.Located.unwrap name)) ~value inner
      |> Option.value ~default:bound)
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

type match_verdict =
  | Bound of (string Data.Located.t * O.Expr.t) list
  | Refuted
  | Undecided

let rec match_pattern (scrutinee : O.Expr.t) (p : O.Pattern.t) : match_verdict =
  let all pairs =
    List.fold_left
      (fun verdict (part, pattern) ->
        match verdict with
        | Refuted -> Refuted
        | Undecided -> begin
            match match_pattern part pattern with
            | Refuted -> Refuted
            | Bound _ | Undecided -> Undecided
          end
        | Bound seen -> begin
            match match_pattern part pattern with
            | Refuted -> Refuted
            | Undecided -> Undecided
            | Bound more -> Bound (seen @ more)
          end)
      (Bound []) pairs
  in
  let rest_of items =
    { O.Expr.typ = scrutinee.typ; expr = O.Expr.Expr_list items }
  in
  match (p.pattern, scrutinee.expr) with
  | O.Pattern.P_T_anything, _ -> Bound []
  | O.Pattern.P_T_var name, _ -> Bound [ (Data.Located.dummy name, scrutinee) ]
  | O.Pattern.P_T_alias _, _ -> Undecided
  | O.Pattern.P_T_unit, _ -> Bound []
  | O.Pattern.P_T_ctor (name, subs), O.Expr.Expr_constr { name = found; arguments }
    when List.length subs = List.length arguments ->
      if Data.Name.equal name found then all (List.combine arguments subs) else Refuted
  | O.Pattern.P_T_ctor (name, _), O.Expr.Expr_constr { name = found; _ } ->
      if Data.Name.equal name found then Undecided else Refuted
  | O.Pattern.P_T_tuple subs, O.Expr.Expr_tuple items
    when List.length subs = List.length items ->
      all (List.combine items subs)
  | O.Pattern.P_T_int wanted, O.Expr.Expr_int found ->
      if wanted = found then Bound [] else Refuted
  | O.Pattern.P_T_str wanted, O.Expr.Expr_string found ->
      if String.equal wanted found then Bound [] else Refuted
  | O.Pattern.P_T_chr wanted, O.Expr.Expr_char found ->
      if String.equal wanted found then Bound [] else Refuted
  | O.Pattern.P_T_list subs, O.Expr.Expr_list items ->
      if List.length subs = List.length items then all (List.combine items subs)
      else Refuted
  | O.Pattern.P_T_list [], O.Expr.Expr_cons _ -> Refuted
  | O.Pattern.P_T_list (first :: rest), O.Expr.Expr_cons { head; tail } ->
      all [ (head, first); (tail, { p with pattern = O.Pattern.P_T_list rest }) ]
  | O.Pattern.P_T_cons (ph, pt), O.Expr.Expr_cons { head; tail } ->
      all [ (head, ph); (tail, pt) ]
  | O.Pattern.P_T_cons _, O.Expr.Expr_list [] -> Refuted
  | O.Pattern.P_T_cons (ph, pt), O.Expr.Expr_list (first :: rest) ->
      all [ (first, ph); (rest_of rest, pt) ]
  | O.Pattern.P_T_record labels, O.Expr.Expr_record rows ->
      let bind label =
        List.find_opt
          (fun (row : O.Expr.expr_record_row) ->
            String.equal row.name label)
          rows
        |> Option.map (fun (row : O.Expr.expr_record_row) ->
               (Data.Located.dummy label, row.value))
      in
      let bound = List.filter_map bind labels in
      if List.length bound = List.length labels then Bound bound else Undecided
  | ( ( O.Pattern.P_T_ctor _ | O.Pattern.P_T_tuple _ | O.Pattern.P_T_int _
      | O.Pattern.P_T_str _ | O.Pattern.P_T_chr _ | O.Pattern.P_T_list _
      | O.Pattern.P_T_cons _ | O.Pattern.P_T_record _ ),
      _ ) ->
      Undecided

let cases_size (cases : O.Expr.expr_pattern_case list) : int =
  List.fold_left
    (fun total (case : O.Expr.expr_pattern_case) ->
      total + pattern_branch_cost + expression_size case.expr)
    0 cases

let cases_free (cases : O.Expr.expr_pattern_case list) : Names.t =
  List.fold_left
    (fun found (case : O.Expr.expr_pattern_case) ->
      Names.union found
        (O.Expr.free_variables ~bound:(O.Pattern.bound case.pattern) case.expr))
    Names.empty cases

let bound_by_cases (cases : O.Expr.expr_pattern_case list) : Names.t =
  List.fold_left
    (fun found (case : O.Expr.expr_pattern_case) ->
      Names.union found (O.Pattern.bound case.pattern))
    Names.empty cases

let settles (case : O.Expr.expr_pattern_case) : bool =
  match case.expr.expr with
  | O.Expr.Expr_constr _ | O.Expr.Expr_int _ | O.Expr.Expr_string _
  | O.Expr.Expr_char _ | O.Expr.Expr_float _ | O.Expr.Expr_unit
  | O.Expr.Expr_ident _ | O.Expr.Expr_tuple _ ->
      true
  | O.Expr.Expr_binop _ | O.Expr.Expr_let _ | O.Expr.Expr_if_then_else _
  | O.Expr.Expr_record _ | O.Expr.Expr_record_update _ | O.Expr.Expr_apply _
  | O.Expr.Expr_pattern _ | O.Expr.Expr_accessor _ | O.Expr.Expr_access _
  | O.Expr.Expr_record_extend _ | O.Expr.Expr_record_select _
  | O.Expr.Expr_record_empty | O.Expr.Expr_kernel _ | O.Expr.Expr_list _
  | O.Expr.Expr_cons _ | O.Expr.Expr_lambda _ ->
      false

let rec reduce_known_matches (e : O.Expr.t) : O.Expr.t =
  let e = O.Expr.map_children e ~f:reduce_known_matches in
  match e.expr with
  | O.Expr.Expr_pattern { expr = scrutinee; pattern_data_items } ->
      let matched body =
        reduce_known_matches
          { e with expr = O.Expr.Expr_pattern { expr = body; pattern_data_items } }
      in
      let fallback () =
        if is_pure scrutinee then pick_known e scrutinee pattern_data_items else e
      in
      begin
        match scrutinee.expr with
        | O.Expr.Expr_let { binding; body } ->
            match_through_let e ~matched ~fallback binding body pattern_data_items
        | O.Expr.Expr_if_then_else branches ->
            match_through_if e ~matched ~fallback branches pattern_data_items
        | O.Expr.Expr_pattern inner ->
            match_through_match e ~matched ~fallback inner pattern_data_items
        | _ -> fallback ()
      end
  | _ -> e

and match_through_let e ~matched ~fallback (binding : O.Expr.expr_let_binding)
    (body : O.Expr.t) cases =
  let bound = Data.Name.local (Data.Located.unwrap binding.bind_body.name) in
  if Names.mem bound (cases_free cases) then fallback ()
  else { e with expr = O.Expr.Expr_let { binding; body = matched body } }

and match_through_if e ~matched ~fallback
    (branches : O.Expr.expr_if_then_else) cases =
  if cases_size cases > inline_size_limit then fallback ()
  else
    {
      e with
      expr =
        O.Expr.Expr_if_then_else
          {
            branches with
            then_exp = matched branches.then_exp;
            else_exp = matched branches.else_exp;
          };
    }

and match_through_match e ~matched ~fallback (inner : O.Expr.expr_pattern) cases =
  let cheap =
    List.for_all settles inner.pattern_data_items
    || cases_size cases <= inline_size_limit
  in
  let capture_free =
    Names.disjoint (bound_by_cases inner.pattern_data_items) (cases_free cases)
  in
  if cheap && capture_free then
    {
      e with
      expr =
        O.Expr.Expr_pattern
          {
            expr = inner.expr;
            pattern_data_items =
              List.map
                (fun (case : O.Expr.expr_pattern_case) ->
                  { case with expr = matched case.expr })
                inner.pattern_data_items;
          };
    }
  else fallback ()

and pick_known e (scrutinee : O.Expr.t) cases =
  match cases with
  | [] -> e
  | (case : O.Expr.expr_pattern_case) :: rest -> begin
      match match_pattern scrutinee case.pattern with
      | Refuted -> pick_known e scrutinee rest
      | Undecided -> e
      | Bound bindings -> bind_arguments bindings case.expr
    end

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

let inlinable_of (d : O.Declaration.t) : inlinable option =
  let free_names = O.Declaration.free d in
  let is_inlinable =
    match d.params with
    | [] -> is_duplicable d.body
    | _ -> expression_size d.body <= inline_size_limit
  in
  if
    is_inlinable
    && not (Names.mem (Data.Name.local (Data.Located.unwrap d.name)) free_names)
  then
    Some
      {
        parameters = d.params;
        parameter_names = O.Declaration.bound d;
        body = d.body;
        free_names;
      }
  else None

let inlinable_declarations (decls : O.Declaration.t list) :
    inlinable By_name.t =
  List.fold_left
    (fun table (d : O.Declaration.t) ->
      match inlinable_of d with
      | Some inlinable ->
          By_name.add
            (Data.Name.local (Data.Located.unwrap d.name))
            inlinable table
      | None -> table)
    By_name.empty decls

module Import = struct
  type t = {
    module_name : string;
    exports : Data.Name.t list;
    declarations : O.Declaration.t list;
  }

  exception Unexportable

  let qualify_name (import : t) (name : Data.Name.t) =
    match name with
    | Data.Name.Global _ -> name
    | Data.Name.Local written ->
        let is_operator = Option.is_some (Data.Operator.referred_to_by name) in
        let is_public =
          List.exists (Data.Name.equal name) import.exports
        in
        if is_operator then name
        else if is_public then
          Data.Name.global ~module_name:import.module_name
            ~exported_name:written
        else raise Unexportable

  let rec qualify_in import ~bound (e : O.Expr.t) : O.Expr.t =
    let inside = qualify_in import in
    match e.expr with
    | Expr_ident name when Names.mem name bound -> e
    | Expr_ident name -> { e with expr = Expr_ident (qualify_name import name) }
    | Expr_constr { name; arguments } ->
        {
          e with
          expr =
            Expr_constr
              {
                name = qualify_name import name;
                arguments = List.map (inside ~bound) arguments;
              };
        }
    | Expr_let { binding; body } ->
        let name =
          Data.Name.local (Data.Located.unwrap binding.bind_body.name)
        in
        let value = inside ~bound binding.bind_body.body in
        {
          e with
          expr =
            Expr_let
              {
                binding = { bind_body = { binding.bind_body with body = value } };
                body = inside ~bound:(Names.add name bound) body;
              };
        }
    | Expr_lambda { params; body } ->
        let bound = Names.union bound (O.Expr.bound_by_lambda params) in
        { e with expr = Expr_lambda { params; body = inside ~bound body } }
    | Expr_pattern _ -> raise Unexportable
    | Expr_binop _ | Expr_if_then_else _ | Expr_record _ | Expr_record_update _
    | Expr_apply _ | Expr_accessor _ | Expr_access _ | Expr_record_extend _
    | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_kernel _
    | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _ | Expr_list _
    | Expr_cons _ | Expr_tuple _ ->
        O.Expr.map_children e ~f:(inside ~bound)

  let candidates (import : t) : (Data.Name.t * inlinable) list =
    List.filter_map
      (fun (d : O.Declaration.t) ->
        let name = Data.Located.unwrap d.name in
        let is_public =
          List.exists (Data.Name.equal (Data.Name.local name)) import.exports
        in
        match inlinable_of d with
        | Some inlinable when is_public -> begin
            match
              qualify_in import ~bound:inlinable.parameter_names inlinable.body
            with
            | body ->
                Some
                  ( Data.Name.global ~module_name:import.module_name
                      ~exported_name:name,
                    {
                      inlinable with
                      body;
                      free_names =
                        O.Expr.free_variables ~bound:inlinable.parameter_names
                          body;
                    } )
            | exception Unexportable -> None
          end
        | Some _ | None -> None)
      import.declarations

  let table (imports : t list) : inlinable By_name.t =
    List.fold_left
      (fun table import ->
        List.fold_left
          (fun table (name, inlinable) -> By_name.add name inlinable table)
          table (candidates import))
      By_name.empty imports
end

let shelter ~taken (bindings : (string Data.Located.t * O.Expr.t) list)
    (body : O.Expr.t) : O.Expr.t =
  let fresh =
    O.Declaration.unused_names ~taken
    |> Seq.take (List.length bindings)
    |> List.of_seq
  in
  let outer =
    List.map2
      (fun name ((parameter : string Data.Located.t), argument) ->
        (Data.Located.at parameter.region name, argument))
      fresh bindings
  in
  let inner =
    List.map2
      (fun (name, _) (parameter, (argument : O.Expr.t)) ->
        ( parameter,
          {
            argument with
            O.Expr.expr = Expr_ident (Data.Name.local (Data.Located.unwrap name));
          } ))
      outer bindings
  in
  bind_arguments outer (bind_arguments inner body)

let rec inline_calls ~(table : inlinable By_name.t) ~(bound : Names.t)
    (e : O.Expr.t) : O.Expr.t =
  let inlined_call () =
    let head, arguments = O.Expr.spine e in
    let arguments = List.map (inline_calls ~table ~bound) arguments in
    let expand candidate =
      let arity = List.length candidate.parameters in
      let taken = List.take arity arguments in
      if Names.disjoint candidate.free_names bound then
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
               let pairs =
                 List.map
                   (fun ((parameter : O.Declaration.param), argument) ->
                     (parameter.name, argument))
                   bindings
               in
               let body = specialise specialisation candidate.body in
               let capture =
                 arguments_escape_binders ~binders:candidate.parameter_names
                   ~arguments:taken
               in
               let expansion =
                 if capture then
                   let taken =
                     List.fold_left
                       (fun taken argument ->
                         Names.union taken
                           (O.Expr.free_variables ~bound:Names.empty argument))
                       (Names.union bound
                          (Names.union candidate.parameter_names
                             candidate.free_names))
                       taken
                   in
                   shelter ~taken pairs body
                 else bind_arguments pairs body
               in
               apply_to expansion (List.drop arity arguments))
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

let optimize ~(imports : Import.t list) (decls : T.Declaration.t list) :
    O.Declaration.t list =
  let import_table = Import.table imports in
  let round decls =
    let table =
      By_name.union
        (fun _ own _ -> Some own)
        (inlinable_declarations decls) import_table
    in
    List.map
      (fun (d : O.Declaration.t) ->
        let bound = O.Declaration.bound d in
        let body =
          d.body
          |> inline_calls ~table ~bound
          |> reduce_beta
          |> reduce_known_matches
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
