let inline_size_limit = 10

(** A value cheap enough to copy into every use site. *)
type atom =
  | Atom_int of int
  | Atom_float of float
  | Atom_string of string
  | Atom_char of string
  | Atom_ident of Data.Name.t
  | Atom_nullary of Data.Name.t
  | Atom_platform of Data.Kernel.platform

let atom_of (e : Optimized.Expr.t) : atom option =
  match e.expr with
  | Expr_int i -> Some (Atom_int i)
  | Expr_float f -> Some (Atom_float f)
  | Expr_string s -> Some (Atom_string s)
  | Expr_char c -> Some (Atom_char c)
  | Expr_ident name -> Some (Atom_ident name)
  | Expr_constr { name; arguments = [] } -> Some (Atom_nullary name)
  | Expr_kernel (Kernel_value (Platform platform)) -> Some (Atom_platform platform)
  | Expr_constr _ | Expr_binop _ | Expr_let _ | Expr_if_then_else _
  | Expr_record _ | Expr_record_update _ | Expr_apply _ | Expr_pattern _
  | Expr_accessor _ | Expr_access _ | Expr_record_extend _
  | Expr_record_select _ | Expr_record_empty | Expr_unit
  | Expr_kernel (Kernel_value (Language _) | Kernel_unary _ | Kernel_binary _)
  | Expr_lambda _ | Expr_list _ | Expr_cons _ | Expr_tuple _ ->
      None

let rec is_pure (e : Optimized.Expr.t) : bool =
  match e.expr with
  | Expr_lambda _ -> true
  | Expr_pattern _ -> false
  | Expr_apply _ ->
      Optimized.Expr.operator_application e
      |> Option.fold ~none:false ~some:(fun (_, left, right) ->
             is_pure left && is_pure right)
  | Expr_constr _ | Expr_binop _ | Expr_let _ | Expr_if_then_else _
  | Expr_record _ | Expr_record_update _ | Expr_ident _ | Expr_accessor _
  | Expr_access _ | Expr_record_extend _ | Expr_record_select _
  | Expr_record_empty | Expr_unit | Expr_kernel _ | Expr_char _ | Expr_string _
  | Expr_int _ | Expr_float _ | Expr_list _ | Expr_cons _ | Expr_tuple _ ->
      List.for_all is_pure (Optimized.Expr.children e)

let rec expression_size (e : Optimized.Expr.t) : int =
  match e.expr with
  | Expr_pattern { expr; pattern_data_items } ->
      1 + expression_size expr + cases_size pattern_data_items
  | Expr_constr _ | Expr_binop _ | Expr_let _ | Expr_if_then_else _
  | Expr_record _ | Expr_record_update _ | Expr_apply _ | Expr_ident _
  | Expr_accessor _ | Expr_access _ | Expr_record_extend _
  | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_kernel _
  | Expr_lambda _ | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _
  | Expr_list _ | Expr_cons _ | Expr_tuple _ ->
      List.fold_left
        (fun acc child -> acc + expression_size child)
        1
        (Optimized.Expr.children e)

and cases_size (cases : Optimized.Expr.expr_pattern_case list) : int =
  let branch_cost = 2 in
  List.fold_left
    (fun total (case : Optimized.Expr.expr_pattern_case) ->
      total + branch_cost + expression_size case.expr)
    0 cases

let arguments_free (arguments : Optimized.Expr.t list) : Data.Name.Set.t =
  Optimized.Expr.union_map
    (Optimized.Expr.free_variables ~bound:Data.Name.Set.empty)
    arguments

(** How often a name is used; a use under a lambda counts as many. *)
type occurrence = Unused | Once | Many

let bind_arguments
    (bindings : (string Data.Located.t * Optimized.Expr.t) list)
    (body : Optimized.Expr.t) : Optimized.Expr.t =
  let substitute ~(name : Data.Name.t) ~(value : Optimized.Expr.t)
        (body : Optimized.Expr.t) : Optimized.Expr.t option =
    let exception Capture in
    let combine left right =
      match (left, right) with
      | Unused, found | found, Unused -> found
      | (Once | Many), (Once | Many) -> Many
    in
    let rec occurrence ~binders (e : Optimized.Expr.t) =
      if Data.Name.Set.mem name binders then Unused
      else
        match e.expr with
        | Expr_ident found -> if Data.Name.equal found name then Once else Unused
        | Expr_lambda _ -> begin
            match below e with Unused -> Unused | Once | Many -> Many
          end
        | _ -> below e
    and below e =
      List.fold_left
        (fun found (binders, child) -> combine found (occurrence ~binders child))
        Unused
        (Optimized.Expr.scoped_children e)
    in
    let outer = Optimized.Expr.free_variables ~bound:Data.Name.Set.empty value in
    let rec replace ~binders (e : Optimized.Expr.t) =
      if Data.Name.Set.mem name binders then e
      else if not (Data.Name.Set.disjoint binders outer) then raise Capture
      else
        match e.expr with
        | Expr_ident found when Data.Name.equal found name ->
            { value with typ = e.typ }
        | _ -> Optimized.Expr.map_scoped e ~f:replace
    in
    let replaced () =
      match replace ~binders:Data.Name.Set.empty body with
      | body -> Some body
      | exception Capture -> None
    in
    match (occurrence ~binders:Data.Name.Set.empty body, atom_of value) with
    | Unused, _ -> Some body
    | Once, _ | Many, Some _ -> replaced ()
    | Many, None -> None
  in
  List.fold_right
    (fun (name, value) (inner : Optimized.Expr.t) ->
      substitute ~name:(Data.Name.local (Data.Located.unwrap name)) ~value inner
      |> Option.value
           ~default:
             {
               Optimized.Expr.typ = inner.typ;
               expr =
                 Expr_let
                   { binding = { bind_body = { name; body = value } }; body = inner };
             })
    bindings body

let propagate_constants (e : Optimized.Expr.t) : Optimized.Expr.t =
  let expr_of_atom : atom -> Optimized.Expr.expr = function
    | Atom_int i -> Expr_int i
    | Atom_float f -> Expr_float f
    | Atom_string s -> Expr_string s
    | Atom_char c -> Expr_char c
    | Atom_ident name -> Expr_ident name
    | Atom_nullary name -> Expr_constr { name; arguments = [] }
    | Atom_platform platform -> Expr_kernel (Kernel_value (Platform platform))
  in
  let boolean value : Optimized.Expr.expr =
    Expr_constr { name = Primitives.bool_constructor value; arguments = [] }
  in
  let fold_primitive (operator : Data.Operator.t) (left : Optimized.Expr.t)
      (right : Optimized.Expr.t) : Optimized.Expr.expr option =
    match (operator, left.expr, right.expr) with
    | Add, Expr_int a, Expr_int b -> Some (Expr_int (a + b))
    | Subtract, Expr_int a, Expr_int b -> Some (Expr_int (a - b))
    | Multiply, Expr_int a, Expr_int b -> Some (Expr_int (a * b))
    | Divide, Expr_int a, Expr_int b when b <> 0 && a mod b = 0 ->
        Some (Expr_int (a / b))
    | Append, Expr_string a, Expr_string b -> Some (Expr_string (a ^ b))
    | Equal, Expr_int a, Expr_int b -> Some (boolean (a = b))
    | Equal, Expr_string a, Expr_string b | Equal, Expr_char a, Expr_char b ->
        Some (boolean (String.equal a b))
    | Less, Expr_int a, Expr_int b -> Some (boolean (a < b))
    | Greater, Expr_int a, Expr_int b -> Some (boolean (a > b))
    | _ -> None
  in
  let forget ~name constants =
    Data.Name.Map.filter
      (fun bound atom ->
        (not (Data.Name.equal bound name))
        &&
        match atom with
        | Atom_ident found -> not (Data.Name.equal found name)
        | Atom_int _ | Atom_float _ | Atom_string _ | Atom_char _
        | Atom_nullary _ | Atom_platform _ ->
            true)
      constants
  in
  let rec propagate ~constants (e : Optimized.Expr.t) : Optimized.Expr.t =
    match e.expr with
    | Expr_ident name ->
        Data.Name.Map.find_opt name constants
        |> Option.fold ~none:e ~some:(fun atom -> { e with expr = expr_of_atom atom })
    | Expr_let { binding; body } ->
        let name = Optimized.Expr.let_name binding in
        let value = propagate ~constants binding.bind_body.body in
        let constants = forget ~name constants in
        let constants =
          atom_of value
          |> Option.fold ~none:constants ~some:(fun atom ->
                 Data.Name.Map.add name atom constants)
        in
        {
          e with
          expr =
            Expr_let
              {
                binding = { bind_body = { binding.bind_body with body = value } };
                body = propagate ~constants body;
              };
        }
    | Expr_if_then_else { if_exp; then_exp; else_exp } -> begin
        let if_exp = propagate ~constants if_exp in
        let known =
          match atom_of if_exp with
          | Some (Atom_nullary name) -> Primitives.bool_of_constructor name
          | Some
              ( Atom_int _ | Atom_float _ | Atom_string _ | Atom_char _
              | Atom_ident _ | Atom_platform _ )
          | None ->
              None
        in
        match known with
        | Some true -> propagate ~constants then_exp
        | Some false -> propagate ~constants else_exp
        | None ->
            {
              e with
              expr =
                Expr_if_then_else
                  {
                    if_exp;
                    then_exp = propagate ~constants then_exp;
                    else_exp = propagate ~constants else_exp;
                  };
            }
      end
    | Expr_apply _ ->
        let inner = Optimized.Expr.map_children e ~f:(propagate ~constants) in
        Optimized.Expr.operator_application inner
        |> Option.map (fun (operator, left, right) -> fold_primitive operator left right)
        |> Option.join
        |> Option.fold ~none:inner ~some:(fun expr -> { inner with expr })
    | _ ->
        Optimized.Expr.map_scoped e ~f:(fun ~binders ->
            propagate
              ~constants:(Data.Name.Set.fold (fun name -> forget ~name) binders constants))
  in
  propagate ~constants:Data.Name.Map.empty e

let reduce_known_matches (e : Optimized.Expr.t) : Optimized.Expr.t =
  let pick_known (scrutinee : Optimized.Expr.t)
        (cases : Optimized.Expr.expr_pattern_case list) : Optimized.Expr.t option =
    let module Verdict = struct
      type t =
        | Bound of (string Data.Located.t * Optimized.Expr.t) list
        | Refuted
        | Undecided
    end in
    let rec match_pattern (scrutinee : Optimized.Expr.t) (p : Optimized.Pattern.t)
        : Verdict.t =
      let combine (left : Verdict.t) (part, pattern) : Verdict.t =
        match (left, match_pattern part pattern) with
        | Refuted, _ | _, Refuted -> Refuted
        | Bound seen, Bound more -> Bound (seen @ more)
        | (Bound _ | Undecided), (Bound _ | Undecided) -> Undecided
      in
      let all pairs = List.fold_left combine (Bound []) pairs in
      let exactly items subs =
        if List.compare_lengths items subs = 0 then all (List.combine items subs)
        else Refuted
      in
      match (p.pattern, scrutinee.expr) with
      | (P_T_anything | P_T_unit), _ -> Bound []
      | P_T_var name, _ -> Bound [ (Data.Located.dummy name, scrutinee) ]
      | P_T_alias _, _ -> Undecided
      | P_T_ctor (name, subs), Expr_constr { name = found; arguments } ->
          if not (Data.Name.equal name found) then Refuted
          else if List.compare_lengths subs arguments = 0 then
            all (List.combine arguments subs)
          else Undecided
      | P_T_tuple subs, Expr_tuple items when List.compare_lengths subs items = 0 ->
          all (List.combine items subs)
      | P_T_int wanted, Expr_int found -> if wanted = found then Bound [] else Refuted
      | P_T_str wanted, Expr_string found | P_T_chr wanted, Expr_char found ->
          if String.equal wanted found then Bound [] else Refuted
      | P_T_list subs, Expr_list items -> exactly items subs
      | P_T_list [], Expr_cons _ | P_T_cons _, Expr_list [] -> Refuted
      | P_T_list (first :: rest), Expr_cons { head; tail } ->
          all [ (head, first); (tail, { p with pattern = P_T_list rest }) ]
      | P_T_cons (ph, pt), Expr_cons { head; tail } -> all [ (head, ph); (tail, pt) ]
      | P_T_cons (ph, pt), Expr_list (first :: rest) ->
          all
            [
              (first, ph);
              ({ Optimized.Expr.typ = scrutinee.typ; expr = Expr_list rest }, pt);
            ]
      | P_T_record labels, Expr_record rows ->
          let bind label =
            List.find_opt
              (fun (row : Optimized.Expr.expr_record_row) -> String.equal row.name label)
              rows
            |> Option.map (fun (row : Optimized.Expr.expr_record_row) ->
                   (Data.Located.dummy label, row.value))
          in
          let bound = List.filter_map bind labels in
          if List.compare_lengths bound labels = 0 then Bound bound else Undecided
      | ( ( P_T_ctor _ | P_T_tuple _ | P_T_int _ | P_T_str _ | P_T_chr _
          | P_T_list _ | P_T_cons _ | P_T_record _ ),
          _ ) ->
          Undecided
    in
    let rec pick = function
      | [] -> None
      | (case : Optimized.Expr.expr_pattern_case) :: rest -> begin
          match match_pattern scrutinee case.pattern with
          | Refuted -> pick rest
          | Undecided -> None
          | Bound bindings -> Some (bind_arguments bindings case.expr)
        end
    in
    pick cases
  in
  let rec reduce_match (e : Optimized.Expr.t) : Optimized.Expr.t =
    match e.expr with
    | Expr_pattern { expr = scrutinee; pattern_data_items = cases } -> begin
        let free =
          Optimized.Expr.union_map
            (fun (case : Optimized.Expr.expr_pattern_case) ->
              Optimized.Expr.free_variables
                ~bound:(Optimized.Pattern.bound case.pattern)
                case.expr)
            cases
        in
        let small = cases_size cases <= inline_size_limit in
        let matched body =
          reduce_match
            { e with expr = Expr_pattern { expr = body; pattern_data_items = cases } }
        in
        let settles (case : Optimized.Expr.expr_pattern_case) =
          match case.expr.expr with
          | Expr_constr _ | Expr_int _ | Expr_string _ | Expr_char _ | Expr_float _
          | Expr_unit | Expr_ident _ | Expr_tuple _ ->
              true
          | Expr_binop _ | Expr_let _ | Expr_if_then_else _ | Expr_record _
          | Expr_record_update _ | Expr_apply _ | Expr_pattern _ | Expr_accessor _
          | Expr_access _ | Expr_record_extend _ | Expr_record_select _
          | Expr_record_empty | Expr_kernel _ | Expr_list _ | Expr_cons _
          | Expr_lambda _ ->
              false
        in
        let capture_free (inner : Optimized.Expr.expr_pattern) =
          Data.Name.Set.disjoint
            (Optimized.Expr.union_map
               (fun (case : Optimized.Expr.expr_pattern_case) ->
                 Optimized.Pattern.bound case.pattern)
               inner.pattern_data_items)
            free
        in
        match scrutinee.expr with
        | Expr_let { binding; body }
          when not (Data.Name.Set.mem (Optimized.Expr.let_name binding) free) ->
            { e with expr = Expr_let { binding; body = matched body } }
        | Expr_if_then_else branches when small ->
            {
              e with
              expr =
                Expr_if_then_else
                  {
                    branches with
                    then_exp = matched branches.then_exp;
                    else_exp = matched branches.else_exp;
                  };
            }
        | Expr_pattern inner
          when (List.for_all settles inner.pattern_data_items || small)
               && capture_free inner ->
            {
              e with
              expr =
                Expr_pattern
                  {
                    expr = inner.expr;
                    pattern_data_items =
                      List.map
                        (fun (case : Optimized.Expr.expr_pattern_case) ->
                          { case with expr = matched case.expr })
                        inner.pattern_data_items;
                  };
            }
        | _ when is_pure scrutinee ->
            pick_known scrutinee cases |> Option.value ~default:e
        | _ -> e
      end
    | _ -> e
  in
  let rec bottom_up (e : Optimized.Expr.t) : Optimized.Expr.t =
    reduce_match (Optimized.Expr.map_children e ~f:bottom_up)
  in
  bottom_up e

let rec reduce_beta (e : Optimized.Expr.t) : Optimized.Expr.t =
  let reduced =
    match Optimized.Expr.spine e with
    | { expr = Expr_lambda { params; body }; typ }, (_ :: _ as arguments) ->
        let arguments = List.map reduce_beta arguments in
        let taken = Int.min (List.length params) (List.length arguments) in
        let bound_params = List.take taken params in
        let bound_arguments = List.take taken arguments in
        if
          Data.Name.Set.disjoint
            (Optimized.Expr.bound_by_lambda bound_params)
            (arguments_free bound_arguments)
        then
          let names =
            List.map
              (fun (p : Optimized.Expr.expr_lambda_param) -> p.name)
              bound_params
          in
          let reduced_body = reduce_beta body in
          let applied_body =
            match List.drop taken params with
            | [] -> reduced_body
            | params ->
                {
                  Optimized.Expr.typ = Optimized.Type.result_after ~applied:taken typ;
                  expr = Expr_lambda { params; body = reduced_body };
                }
          in
          Some
            (Optimized.Expr.apply
               (bind_arguments (List.combine names bound_arguments) applied_body)
               (List.drop taken arguments))
        else None
    | { expr = Expr_let { binding; body }; _ }, (_ :: _ as arguments)
      when not
             (Data.Name.Set.mem
                (Optimized.Expr.let_name binding)
                (arguments_free arguments)) ->
        Some
          {
            e with
            expr = Expr_let { binding; body = Optimized.Expr.apply body arguments };
          }
    | _ -> None
  in
  match reduced with
  | Some result -> result
  | None -> Optimized.Expr.map_children e ~f:reduce_beta

let rec eliminate_dead_lets (e : Optimized.Expr.t) : Optimized.Expr.t =
  let e = Optimized.Expr.map_children e ~f:eliminate_dead_lets in
  match e.expr with
  | Expr_let { binding; body }
    when is_pure binding.bind_body.body
         && not
              (Data.Name.Set.mem
                 (Optimized.Expr.let_name binding)
                 (Optimized.Expr.free_variables ~bound:Data.Name.Set.empty body))
    ->
      body
  | _ -> e

let inlinable_of (d : Optimized.Declaration.t) : Optimized.Declaration.t option =
  let small =
    match d.params with
    | [] -> Option.is_some (atom_of d.body)
    | _ :: _ -> expression_size d.body <= inline_size_limit
  in
  if
    small
    && not
         (Data.Name.Set.mem
            (Optimized.Declaration.name_of d)
            (Optimized.Declaration.free d))
  then Some d
  else None

module Import = struct
  type t = {
    module_name : string;
    exports : Data.Name.t list;
    declarations : Optimized.Declaration.t list;
  }

  let unfoldings (import : t) : (Data.Name.t * Optimized.Declaration.t) list =
    let exception Unexportable in
    let exported = Data.Name.Set.of_list import.exports in
    let qualified written =
      Data.Name.global ~module_name:import.module_name ~exported_name:written
    in
    let qualify (name : Data.Name.t) =
      match name with
      | Global _ -> name
      | Local _ when Option.is_some (Data.Operator.referred_to_by name) -> name
      | Local written when Data.Name.Set.mem name exported -> qualified written
      | Local _ -> raise Unexportable
    in
    let rec qualify_in ~bound (e : Optimized.Expr.t) : Optimized.Expr.t =
      match e.expr with
      | Expr_ident name when not (Data.Name.Set.mem name bound) ->
          { e with expr = Expr_ident (qualify name) }
      | Expr_constr { name; arguments } ->
          {
            e with
            expr =
              Expr_constr
                { name = qualify name; arguments = List.map (qualify_in ~bound) arguments };
          }
      | Expr_pattern _ -> raise Unexportable
      | _ ->
          Optimized.Expr.map_scoped e ~f:(fun ~binders ->
              qualify_in ~bound:(Data.Name.Set.union bound binders))
    in
    let unfolding (d : Optimized.Declaration.t) =
      match qualify_in ~bound:(Optimized.Declaration.bound d) d.body with
      | body -> Some (qualified (Data.Located.unwrap d.name), { d with body })
      | exception Unexportable -> None
    in
    List.filter_map
      (fun (d : Optimized.Declaration.t) ->
        if Data.Name.Set.mem (Optimized.Declaration.name_of d) exported then
          Option.bind (inlinable_of d) unfolding
        else None)
      import.declarations
end

let inline_calls ~(table : Optimized.Declaration.t Data.Name.Map.t)
    ~(bound : Data.Name.Set.t) (e : Optimized.Expr.t) : Optimized.Expr.t =
  let rec match_types bindings (parameter : Optimized.Type.t)
      (argument : Optimized.Type.t) =
    match (parameter, argument) with
    | TVar variable, _ ->
        if Optimized.Type.By_variable.mem variable bindings then bindings
        else Optimized.Type.By_variable.add variable argument bindings
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
  in
  let rec specialise bindings (e : Optimized.Expr.t) : Optimized.Expr.t =
    let e = Optimized.Expr.map_children e ~f:(specialise bindings) in
    let expr =
      match e.expr with
      | Expr_lambda { params; body } ->
          Optimized.Expr.Expr_lambda
            {
              params =
                List.map
                  (fun (p : Optimized.Expr.expr_lambda_param) ->
                    { p with typ = Optimized.Type.substitute bindings p.typ })
                  params;
              body;
            }
      | Expr_pattern { expr; pattern_data_items } ->
          Expr_pattern
            {
              expr;
              pattern_data_items =
                List.map
                  (fun (case : Optimized.Expr.expr_pattern_case) ->
                    { case with pattern = Optimized.Pattern.substitute bindings case.pattern })
                  pattern_data_items;
            }
      | untyped -> untyped
    in
    { typ = Optimized.Type.substitute bindings e.typ; expr }
  in
  let shelter ~taken (pairs : (string Data.Located.t * Optimized.Expr.t) list) body =
    let renamed =
      Seq.zip (Optimized.Declaration.unused_names ~taken) (List.to_seq pairs)
      |> List.of_seq
    in
    let outer =
      List.map
        (fun (fresh, ((parameter : string Data.Located.t), argument)) ->
          (Data.Located.at parameter.region fresh, argument))
        renamed
    in
    let inner =
      List.map
        (fun (fresh, (parameter, (argument : Optimized.Expr.t))) ->
          (parameter, { argument with expr = Expr_ident (Data.Name.local fresh) }))
        renamed
    in
    bind_arguments outer (bind_arguments inner body)
  in
  let expand ~bound (candidate : Optimized.Declaration.t) arguments =
    if List.compare_lengths arguments candidate.params < 0 then None
    else
      let arity = List.length candidate.params in
      let taken = List.take arity arguments in
      let binders = Optimized.Declaration.bound candidate in
      let specialisation =
        matched_all Optimized.Type.By_variable.empty
          (List.map (fun (p : Optimized.Declaration.param) -> p.typ) candidate.params)
          (List.map (fun (a : Optimized.Expr.t) -> a.typ) taken)
      in
      let pairs =
        List.map2
          (fun (p : Optimized.Declaration.param) argument -> (p.name, argument))
          candidate.params taken
      in
      let body = specialise specialisation candidate.body in
      let argument_names = arguments_free taken in
      let expansion =
        if Data.Name.Set.disjoint binders argument_names then bind_arguments pairs body
        else
          shelter
            ~taken:
              (Data.Name.Set.union bound
                 (Data.Name.Set.union binders
                    (Data.Name.Set.union
                       (Optimized.Declaration.free candidate)
                       argument_names)))
            pairs body
      in
      Some (Optimized.Expr.apply expansion (List.drop arity arguments))
  in
  let rec inline ~bound (e : Optimized.Expr.t) : Optimized.Expr.t =
    let candidate name =
      if Data.Name.Set.mem name bound then None
      else
        Option.bind (Data.Name.Map.find_opt name table) (fun d ->
            if Data.Name.Set.disjoint (Optimized.Declaration.free d) bound then Some d
            else None)
    in
    match e.expr with
    | Expr_ident _ | Expr_apply _ -> begin
        let head, arguments = Optimized.Expr.spine e in
        let inlined =
          Option.bind
            (Option.bind (Optimized.Expr.ident_of head) candidate)
            (fun d -> expand ~bound d (List.map (inline ~bound) arguments))
        in
        match inlined with
        | Some result -> result
        | None -> Optimized.Expr.map_children e ~f:(inline ~bound)
      end
    | _ ->
        Optimized.Expr.map_scoped e ~f:(fun ~binders ->
            inline ~bound:(Data.Name.Set.union bound binders))
  in
  inline ~bound e

let optimize ~(imports : Import.t list) (decls : Typed.Declaration.t list) :
    Optimized.Declaration.t list =
  let import_table =
    Data.Name.Map.of_list (List.concat_map Import.unfoldings imports)
  in
  let round decls =
    let own (d : Optimized.Declaration.t) =
      Option.map
        (fun unfolding -> (Optimized.Declaration.name_of d, unfolding))
        (inlinable_of d)
    in
    let table =
      Data.Name.Map.add_seq (List.to_seq (List.filter_map own decls)) import_table
    in
    List.map
      (fun (d : Optimized.Declaration.t) ->
        let body =
          d.body
          |> inline_calls ~table ~bound:(Optimized.Declaration.bound d)
          |> reduce_beta |> reduce_known_matches |> propagate_constants
          |> eliminate_dead_lets
        in
        { d with body })
      decls
  in
  let rec rounds ~remaining decls =
    if remaining = 0 then decls else rounds ~remaining:(remaining - 1) (round decls)
  in
  List.map
    (fun d -> Optimized.Declaration.saturate (Optimized.Declaration.of_typed d))
    decls
  |> rounds ~remaining:3
