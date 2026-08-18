open Typed
open Typed.Type
open Canonical.Expr

let ftv_typ ty =
  Type.fold_variables
    (fun collected variable -> Variables.add variable collected)
    Variables.empty ty

let deeper_than_the_binding ty =
  let bound_here = Variable.current_level () in
  Variables.elements
    (Type.fold_variables
       (fun collected variable ->
         match Variable.state variable with
         | Variable.Linked _ -> collected
         | Variable.Unbound { level; _ } ->
             if level > bound_here then Variables.add variable collected
             else collected)
       Variables.empty ty)

let generalize ty = Scheme (deeper_than_the_binding ty, Type.zonk ty)

let infer_deeper compute =
  Variable.enter_level ();
  Fun.protect ~finally:Variable.leave_level compute

let instantiate = function
  | Scheme ([], ty) -> ty
  | Scheme (quantified, ty) ->
      let copies =
        List.fold_left
          (fun collected variable ->
            By_variable.add variable
              (fresh_variable (Variable.constraint_of variable))
              collected)
          By_variable.empty quantified
      in
      substitute copies ty

let rec infer_pattern (type_env : Type_env.t) (pattern : Canonical.Pattern.t) :
    Typed.Pattern.t * Value_env.t =
  let go = infer_pattern type_env in
  let in_order patterns =
    let reversed, bound =
      List.fold_left
        (fun (inferred, bound) pattern ->
          let typed, bound_here = go pattern in
          (typed :: inferred, Value_env.binders_of_both bound bound_here))
        ([], Value_env.empty) patterns
    in
    (List.rev reversed, bound)
  in
  match pattern with
  | Canonical.Pattern.P_var name ->
      let bound_type = fresh_variable None in
      ({ Pattern.typ = bound_type; pattern = P_T_var name },
        Value_env.bind_one name bound_type Value_env.empty)
  | P_anything -> ({ typ = fresh_variable None; pattern = P_T_anything }, Value_env.empty)
  | P_int value ->
      ({ typ = fresh_variable (Some Data.Constraint.Number); pattern = P_T_int value }, Value_env.empty)
  | P_str text -> ({ typ = TStr; pattern = P_T_str text }, Value_env.empty)
  | P_chr letter -> ({ typ = TChar; pattern = P_T_chr letter }, Value_env.empty)
  | P_unit -> ({ typ = TUnit; pattern = P_T_unit }, Value_env.empty)
  | P_tuple items ->
      let inferred, bound = in_order items in
      ( {
          typ = TTup (List.map (fun typed -> typed.Pattern.typ) inferred);
          pattern = P_T_tuple inferred;
        },
        bound )
  | P_list items ->
      let element = fresh_variable None in
      let inferred, bound = in_order items in
      List.iter (fun typed -> Unify.types element typed.Pattern.typ) inferred;
      ({ typ = list_of element; pattern = P_T_list inferred }, bound)
  | P_alias (inner, name) ->
      let aliased, bound = go inner in
      let typ = aliased.Pattern.typ in
      ( { typ; pattern = P_T_alias (aliased, name) },
        Value_env.bind_one name typ bound )
  | P_cons (head, tail) ->
      let typed_head, head_bound = go head in
      let typed_tail, tail_bound = go tail in
      let list_type = list_of typed_head.Pattern.typ in
      Unify.types typed_tail.typ list_type;
      ( { typ = list_type; pattern = P_T_cons (typed_head, typed_tail) },
        Value_env.binders_of_both head_bound tail_bound )
  | P_ctor (name, arguments) -> begin
      match Type_env.constructor_of name type_env with
      | None -> Message.fail "Unknown constructor %s" (Data.Name.to_string name)
      | Some (declared, ctor) ->
          let wrong_arity () =
            let takes = List.length ctor.data in
            Message.fail
              "Constructor %s takes %d argument%s, the pattern gives %d"
              (Data.Name.to_string name) takes
              (if takes = 1 then "" else "s")
              (List.length arguments)
          in
          let rec against_payloads (inferred, bound) arguments carried =
            match (arguments, carried) with
            | argument :: rest, TFun (payload, remaining) ->
                let typed, bound_here = go argument in
                Unify.types typed.Pattern.typ payload;
                against_payloads
                  (typed :: inferred, Value_env.binders_of_both bound bound_here)
                  rest remaining
            | [], TFun _ | _ :: _, _ -> wrong_arity ()
            | [], matched -> (inferred, bound, matched)
          in
          let inferred, bound, matched =
            against_payloads ([], Value_env.empty) arguments
              (instantiate (Type_env.constructor_scheme declared ctor))
          in
          ({ typ = matched; pattern = P_T_ctor (name, List.rev inferred) }, bound)
    end
  | P_record fields ->
      let row_var = fresh_variable None in
      let row, bound =
        List.fold_right
          (fun field (row, bound) ->
            let field_type = fresh_variable None in
            ( TRowExtend (field, field_type, row),
              Value_env.bind_one field field_type bound ))
          fields (row_var, Value_env.empty)
      in
      ({ typ = TRecord row; pattern = P_T_record fields }, bound)

let assume_parameters ctx params =
  let assumed = List.map (fun _ -> fresh_variable None) params in
  let visible =
    List.fold_left2
      (fun visible param typ ->
        Value_env.bind_one param.Data.Located.thing typ visible)
      ctx params assumed
  in
  (assumed, visible)

let matching_the_annotation type_env subject = function
  | None -> ()
  | Some written ->
      Unify.types subject
        (Type_env.expand type_env (Type_env.written_type written))

type matched_so_far = {
  result_type : Type.t;
  branches : Typed.Expr.expr_pattern_case list;
}

let rec infer_with_env (exp : Canonical.Expr.t) ctx (type_env : Type_env.t) :
    Typed.Expr.t =
  let infer exp ctx = infer_with_env exp ctx type_env in
  let go exp = infer exp ctx in
  let node expr typ = { Typed.Expr.expr; typ } in
  match exp with
  | Expr_int value -> node (Typed.Expr.Expr_int value) (fresh_variable (Some Data.Constraint.Number))
  | Expr_float value -> node (Expr_float value) TFloat
  | Expr_string text -> node (Expr_string text) TStr
  | Expr_char letter -> node (Expr_char letter) TChar
  | Expr_unit -> node Typed.Expr.Expr_unit TUnit
  | Expr_kernel primitive -> node (Typed.Expr.Expr_kernel primitive) (fresh_variable None)
  | Expr_ident name -> begin
      match Value_env.find name ctx with
      | None -> Message.fail "Unbound value %s" (Data.Name.to_string name)
      | Some scheme -> node (Expr_ident name) (instantiate scheme)
    end
  | Expr_apply { fn; arg } ->
      let typed_callee = go fn in
      let typed_argument = go arg in
      let result = fresh_variable None in
      Unify.types typed_callee.typ (TFun (typed_argument.typ, result));
      node (Expr_apply { fn = typed_callee; arg = typed_argument }) result
  | Expr_if_then_else { if_exp; then_exp; else_exp } ->
      let typed_condition = go if_exp in
      Unify.types typed_condition.typ TBool;
      let typed_then = go then_exp in
      let typed_else = go else_exp in
      Unify.types typed_then.typ typed_else.typ;
      node
        (Expr_if_then_else
           {
             if_exp = typed_condition;
             then_exp = typed_then;
             else_exp = typed_else;
           })
        typed_else.typ
  | Expr_list items ->
      let element = fresh_variable None in
      let inferred =
        List.map
          (fun item ->
            let typed = go item in
            Unify.types element typed.Typed.Expr.typ;
            typed)
          items
      in
      node (Expr_list inferred) (list_of element)
  | Expr_record_update { record; fields } ->
      let typed_record = go record in
      let inferred =
        List.map
          (fun (row : Canonical.Expr.expr_record_row) ->
            let typed_value = go row.value in
            let others = fresh_variable None in
            Unify.types typed_record.typ
              (TRecord (TRowExtend (row.name, typed_value.typ, others)));
            { Typed.Expr.name = row.name; value = typed_value })
          fields
      in
      node
        (Expr_record_update { record = typed_record; fields = inferred })
        typed_record.typ
  | Expr_tuple items ->
      let inferred = List.map go items in
      node (Expr_tuple inferred)
        (TTup (List.map (fun (item : Typed.Expr.t) -> item.typ) inferred))
  | Expr_cons { head; tail } ->
      let typed_head = go head in
      let typed_tail = go tail in
      Unify.types typed_tail.typ (list_of typed_head.typ);
      node (Expr_cons { head = typed_head; tail = typed_tail }) typed_tail.typ
  | Expr_let { binding = { bind_type; bind_body = { name; body = rhs } }; body }
    ->
      let typed_bound =
        infer_deeper (fun () ->
            let assumed = fresh_variable None in
            let visible_to_itself = Value_env.bind_one name.thing assumed ctx in
            let typed_bound = infer rhs visible_to_itself in
            Unify.types assumed typed_bound.typ;
            matching_the_annotation type_env typed_bound.typ
              (Option.map (fun annotation -> annotation.content) bind_type);
            typed_bound)
      in
      let visible_to_the_body =
        Value_env.bind
          (Data.Name.local name.thing)
          (generalize typed_bound.typ) ctx
      in
      let typed_body = infer body visible_to_the_body in
      node
        (Expr_let
           {
             binding = { bind_body = { name; body = typed_bound } };
             body = typed_body;
           })
        typed_body.typ
  | Expr_pattern { expr; pattern_data_items } -> begin
      let branch scrutinee matched { pattern; expr = branch_expr } =
        let typed_pattern, bound = infer_pattern type_env pattern in
        Unify.types typed_pattern.Pattern.typ scrutinee;
        let visible_in_the_branch = Value_env.shadow ~by:bound ctx in
        let typed_branch = infer branch_expr visible_in_the_branch in
        Unify.types typed_branch.typ matched.result_type;
        {
          result_type = matched.result_type;
          branches =
            { Typed.Expr.pattern = typed_pattern; expr = typed_branch }
            :: matched.branches;
        }
      in
      match pattern_data_items with
      | [] -> Message.fail "A case expression needs at least one branch"
      | written ->
          let scrutinee = go expr in
          let start = { result_type = fresh_variable None; branches = [] } in
          let matched = List.fold_left (branch scrutinee.typ) start written in
          node
            (Expr_pattern
               {
                 expr = scrutinee;
                 pattern_data_items = List.rev matched.branches;
               })
            matched.result_type
    end
  | Expr_record_extend label ->
      let field = fresh_variable None in
      let row = fresh_variable None in
      node
        (Expr_record_extend label)
        (TFun
           (field, TFun (TRecord row, TRecord (TRowExtend (label, field, row)))))
  | Expr_record_empty -> node Expr_record_empty (TRecord TRowEmpty)
  | Expr_record_select label ->
      let field = fresh_variable None in
      let row = fresh_variable None in
      node
        (Expr_record_select label)
        (TFun (TRecord (TRowExtend (label, field, row)), field))
  | Expr_accessor written ->
      let field = fresh_variable None in
      let row = fresh_variable None in
      let label = Data.Located.unwrap written in
      node (Expr_accessor written)
        (TFun (TRecord (TRowExtend (label, field, row)), field))
  | Expr_access { expr; field } ->
      let typed_record = go expr in
      let selected = fresh_variable None in
      let row = fresh_variable None in
      Unify.types typed_record.typ
        (TRecord (TRowExtend (field.thing, selected, row)));
      node (Expr_access { expr = typed_record; field }) selected
  | Expr_lambda { params; body } ->
      let assumed, visible_in_the_body = assume_parameters ctx params in
      let typed_body = infer body visible_in_the_body in
      node
        (Expr_lambda
           {
             params =
               List.map2
                 (fun name typ -> { Typed.Expr.name; typ })
                 params assumed;
             body = typed_body;
           })
        (function_of assumed ~result:typed_body.typ)

