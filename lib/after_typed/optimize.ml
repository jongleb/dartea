let inline_size_limit = 10

module Atom = struct
  (** A value cheap enough to copy into every use site. *)
  type t =
    | Int of int
    | Float of float
    | String of string
    | Char of string
    | Ident of Data.Name.t
    | Nullary of Data.Name.t
    | Platform of Data.Kernel.platform

  let of_expr (e : Optimized.Expr.t) : t option =
    match e.expr with
    | Expr_int i -> Some (Int i)
    | Expr_float f -> Some (Float f)
    | Expr_string s -> Some (String s)
    | Expr_char c -> Some (Char c)
    | Expr_ident name -> Some (Ident name)
    | Expr_constr { name; arguments = [] } -> Some (Nullary name)
    | Expr_kernel (Kernel_value (Platform platform)) -> Some (Platform platform)
    | Expr_constr _ | Expr_binop _ | Expr_let _ | Expr_if_then_else _
    | Expr_record _ | Expr_record_update _ | Expr_apply _ | Expr_pattern _
    | Expr_accessor _ | Expr_access _ | Expr_record_extend _
    | Expr_record_select _ | Expr_record_empty | Expr_unit
    | Expr_kernel (Kernel_value (Language _) | Kernel_unary _ | Kernel_binary _)
    | Expr_lambda _ | Expr_list _ | Expr_cons _ | Expr_tuple _ ->
        None

  let to_expr : t -> Optimized.Expr.expr = function
    | Int i -> Expr_int i
    | Float f -> Expr_float f
    | String s -> Expr_string s
    | Char c -> Expr_char c
    | Ident name -> Expr_ident name
    | Nullary name -> Expr_constr { name; arguments = [] }
    | Platform platform -> Expr_kernel (Kernel_value (Platform platform))

  let mentions name = function
    | Ident found -> Data.Name.equal found name
    | Int _ | Float _ | String _ | Char _ | Nullary _ | Platform _ -> false
end

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
        (fun found (binders, child) ->
          match found with
          | Many -> Many
          | Unused -> occurrence ~binders child
          | Once -> begin
              match occurrence ~binders child with
              | Unused -> Once
              | Once | Many -> Many
            end)
        Unused
        (Optimized.Expr.scoped_children e)
    in
    let replaced () =
      let outer =
        Optimized.Expr.free_variables ~bound:Data.Name.Set.empty value
      in
      let rec replace ~binders (e : Optimized.Expr.t) =
        if Data.Name.Set.mem name binders then e
        else if not (Data.Name.Set.disjoint binders outer) then raise Capture
        else
          match e.expr with
          | Expr_ident found when Data.Name.equal found name ->
              { value with typ = e.typ }
          | _ -> Optimized.Expr.map_scoped e ~f:replace
      in
      match replace ~binders:Data.Name.Set.empty body with
      | body -> Some body
      | exception Capture -> None
    in
    match (occurrence ~binders:Data.Name.Set.empty body, Atom.of_expr value) with
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
  let forget ~names constants =
    if Data.Name.Set.is_empty names then constants
    else
      Data.Name.Map.filter
        (fun bound atom ->
          (not (Data.Name.Set.mem bound names))
          && not (Data.Name.Set.exists (fun name -> Atom.mentions name atom) names))
        constants
  in
  let rec propagate ~constants (e : Optimized.Expr.t) : Optimized.Expr.t =
    match e.expr with
    | Expr_ident name ->
        Data.Name.Map.find_opt name constants
        |> Option.fold ~none:e ~some:(fun atom -> { e with expr = Atom.to_expr atom })
    | Expr_let { binding; body } ->
        let name = Optimized.Expr.let_name binding in
        let value = propagate ~constants binding.bind_body.body in
        let constants = forget ~names:(Data.Name.Set.singleton name) constants in
        let constants =
          Atom.of_expr value
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
          Option.bind (Atom.of_expr if_exp) (function
            | Atom.Nullary name -> Primitives.bool_of_constructor name
            | Int _ | Float _ | String _ | Char _ | Ident _ | Platform _ -> None)
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
        Option.bind (Optimized.Expr.operator_application inner)
          (fun (operator, left, right) -> fold_primitive operator left right)
        |> Option.fold ~none:inner ~some:(fun expr -> { inner with expr })
    | _ ->
        Optimized.Expr.map_scoped e ~f:(fun ~binders ->
            propagate ~constants:(forget ~names:binders constants))
  in
  propagate ~constants:Data.Name.Map.empty e

(** What matching a known scrutinee against a pattern tells. *)
type verdict =
  | Bound of (string Data.Located.t * Optimized.Expr.t) list
  | Refuted
  | Undecided

let reduce_known_matches (e : Optimized.Expr.t) : Optimized.Expr.t =
  let rec match_pattern (scrutinee : Optimized.Expr.t) (p : Optimized.Pattern.t) =
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
        all [ (first, ph); ({ scrutinee with expr = Expr_list rest }, pt) ]
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
  and all pairs =
    List.fold_left
      (fun left (part, pattern) ->
        match (left, match_pattern part pattern) with
        | Refuted, _ | _, Refuted -> Refuted
        | Bound seen, Bound more -> Bound (seen @ more)
        | (Bound _ | Undecided), (Bound _ | Undecided) -> Undecided)
      (Bound []) pairs
  in
  let rec pick_known scrutinee = function
    | [] -> None
    | (case : Optimized.Expr.expr_pattern_case) :: rest -> begin
        match match_pattern scrutinee case.pattern with
        | Refuted -> pick_known scrutinee rest
        | Undecided -> None
        | Bound bindings -> Some (bind_arguments bindings case.expr)
      end
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
  let reduce_match (e : Optimized.Expr.t) (matching : Optimized.Expr.expr_pattern) =
    let cases = matching.pattern_data_items in
    let free =
      lazy
        (Optimized.Expr.union_map
           (fun (case : Optimized.Expr.expr_pattern_case) ->
             Optimized.Expr.free_variables
               ~bound:(Optimized.Pattern.bound case.pattern)
               case.expr)
           cases)
    in
    let small = lazy (cases_size cases <= inline_size_limit) in
    let rec push (scrutinee : Optimized.Expr.t) : Optimized.Expr.t =
      match scrutinee.expr with
      | Expr_let { binding; body }
        when not
               (Data.Name.Set.mem
                  (Optimized.Expr.let_name binding)
                  (Lazy.force free)) ->
          { e with expr = Expr_let { binding; body = push body } }
      | Expr_if_then_else branches when Lazy.force small ->
          {
            e with
            expr =
              Expr_if_then_else
                {
                  branches with
                  then_exp = push branches.then_exp;
                  else_exp = push branches.else_exp;
                };
          }
      | Expr_pattern inner
        when (List.for_all settles inner.pattern_data_items || Lazy.force small)
             && Data.Name.Set.disjoint
                  (Optimized.Expr.union_map
                     (fun (case : Optimized.Expr.expr_pattern_case) ->
                       Optimized.Pattern.bound case.pattern)
                     inner.pattern_data_items)
                  (Lazy.force free) ->
          {
            e with
            expr =
              Expr_pattern
                {
                  inner with
                  pattern_data_items =
                    List.map
                      (fun (case : Optimized.Expr.expr_pattern_case) ->
                        { case with expr = push case.expr })
                      inner.pattern_data_items;
                };
          }
      | _ ->
          let whole =
            { e with expr = Expr_pattern { matching with expr = scrutinee } }
          in
          if is_pure scrutinee then
            pick_known scrutinee cases |> Option.value ~default:whole
          else whole
    in
    push matching.expr
  in
  let rec bottom_up (e : Optimized.Expr.t) : Optimized.Expr.t =
    let e = Optimized.Expr.map_children e ~f:bottom_up in
    match e.expr with
    | Expr_pattern matching -> reduce_match e matching
    | _ -> e
  in
  bottom_up e

let rec reduce_beta (e : Optimized.Expr.t) : Optimized.Expr.t =
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
        let bindings =
          List.map2
            (fun (p : Optimized.Expr.expr_lambda_param) argument -> (p.name, argument))
            bound_params bound_arguments
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
        Optimized.Expr.apply
          (bind_arguments bindings applied_body)
          (List.drop taken arguments)
      else Optimized.Expr.map_children e ~f:reduce_beta
  | { expr = Expr_let { binding; body }; _ }, (_ :: _ as arguments)
    when not
           (Data.Name.Set.mem
              (Optimized.Expr.let_name binding)
              (arguments_free arguments)) ->
      { e with expr = Expr_let { binding; body = Optimized.Expr.apply body arguments } }
  | _ -> Optimized.Expr.map_children e ~f:reduce_beta

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

(** A declaration small enough to copy to its call sites, with the names it
    binds and the names it refers to. *)
type unfolding = {
  declaration : Optimized.Declaration.t;
  binders : Data.Name.Set.t;
  free : Data.Name.Set.t;
}

let unfolding_of (declaration : Optimized.Declaration.t) : unfolding =
  {
    declaration;
    binders = Optimized.Declaration.bound declaration;
    free = Optimized.Declaration.free declaration;
  }

let inlinable_of (d : Optimized.Declaration.t) : Optimized.Declaration.t option =
  let small =
    match d.params with
    | [] -> Option.is_some (Atom.of_expr d.body)
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

  let unfoldings (import : t) : (Data.Name.t * unfolding) list =
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
      | body ->
          Some (qualified (Data.Located.unwrap d.name), unfolding_of { d with body })
      | exception Unexportable -> None
    in
    List.filter_map
      (fun (d : Optimized.Declaration.t) ->
        if Data.Name.Set.mem (Optimized.Declaration.name_of d) exported then
          Option.bind (inlinable_of d) unfolding
        else None)
      import.declarations
end

let inline_calls ~(table : unfolding Data.Name.Map.t) ~(bound : Data.Name.Set.t)
    (e : Optimized.Expr.t) : Optimized.Expr.t =
  let shelter ~taken (pairs : (string Data.Located.t * Optimized.Expr.t) list) body =
    let outer, inner =
      Optimized.Declaration.with_fresh_names ~taken pairs
      |> List.map
           (fun (fresh, ((parameter : string Data.Located.t), (argument : Optimized.Expr.t))) ->
             ( (Data.Located.at parameter.region fresh, argument),
               (parameter, { argument with expr = Expr_ident (Data.Name.local fresh) }) ))
      |> List.split
    in
    bind_arguments outer (bind_arguments inner body)
  in
  let expand ~bound (candidate : unfolding) arguments =
    let params = candidate.declaration.params in
    let arity = List.length params in
    let taken = List.take arity arguments in
    let specialisation =
      Optimized.Type.match_all Optimized.Type.By_variable.empty
        (List.map (fun (p : Optimized.Declaration.param) -> p.typ) params)
        (List.map (fun (a : Optimized.Expr.t) -> a.typ) taken)
    in
    let pairs =
      List.map2
        (fun (p : Optimized.Declaration.param) argument -> (p.name, argument))
        params taken
    in
    let body =
      if Optimized.Type.By_variable.is_empty specialisation then
        candidate.declaration.body
      else Optimized.Expr.substitute_types specialisation candidate.declaration.body
    in
    let argument_names = arguments_free taken in
    let expansion =
      if Data.Name.Set.disjoint candidate.binders argument_names then
        bind_arguments pairs body
      else
        shelter
          ~taken:
            (List.fold_left Data.Name.Set.union bound
               [ candidate.binders; candidate.free; argument_names ])
          pairs body
    in
    Optimized.Expr.apply expansion (List.drop arity arguments)
  in
  let rec inline ~bound (e : Optimized.Expr.t) : Optimized.Expr.t =
    match e.expr with
    | Expr_ident _ | Expr_apply _ -> begin
        let head, arguments = Optimized.Expr.spine e in
        let candidate name =
          if Data.Name.Set.mem name bound then None
          else
            Option.bind (Data.Name.Map.find_opt name table) (fun candidate ->
                if
                  Data.Name.Set.disjoint candidate.free bound
                  && List.compare_lengths arguments candidate.declaration.params >= 0
                then Some candidate
                else None)
        in
        match Option.bind (Optimized.Expr.ident_of head) candidate with
        | Some candidate -> expand ~bound candidate (List.map (inline ~bound) arguments)
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
        (fun d -> (Optimized.Declaration.name_of d, unfolding_of d))
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
