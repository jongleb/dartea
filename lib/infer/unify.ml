open Typed
open Typed.Type

let combine left right =
  match Data.Constraint.combined left right with
  | Some together -> together
  | None ->
      Message.fail "%s and %s cannot be the same type variable"
        (Data.Constraint.name left) (Data.Constraint.name right)

let narrow required variable =
  match Variable.constraint_of variable with
  | None -> Variable.constrain variable required
  | Some carried ->
      let together = combine carried required in
      if together <> carried then Variable.constrain variable together

let list_element_of name arguments =
  match arguments with
  | [ element ] when Data.Name.equal name (Data.Name.local "List") ->
      Some element
  | _ -> None

let rec satisfy (required : Data.Constraint.t) ty =
  let unsatisfied () =
    Message.fail "%s does not satisfy %s" (Message.of_type ty)
      (Data.Constraint.name required)
  in
  match (required, Type.head ty) with
  | _, TVar variable -> narrow required variable
  | Number, (TInt | TFloat) -> ()
  | Comparable, (TInt | TFloat | TChar | TStr) -> ()
  | (Appendable | Comp_appendable), TStr -> ()
  | Comparable, TTup items -> List.iter (satisfy Comparable) items
  | ( (Number | Appendable | Comparable | Comp_appendable),
      TCustom (name, arguments) ) -> begin
      match (required, list_element_of name arguments) with
      | _, None -> unsatisfied ()
      | Appendable, Some _ -> ()
      | (Comparable | Comp_appendable), Some element ->
          satisfy Comparable element
      | Number, Some _ -> unsatisfied ()
    end
  | ( (Number | Comparable | Appendable | Comp_appendable),
      ( TInt | TFloat | TChar | TStr | TBool | TUnit | TFun _ | TTup _
      | TRecord _ | TRowExtend _ | TRowEmpty ) ) ->
      unsatisfied ()

let constraint_of_both one other =
  match (Variable.constraint_of one, Variable.constraint_of other) with
  | None, carried -> carried
  | carried, None -> carried
  | Some carried, Some carried_other -> Some (combine carried carried_other)

let admit variable ty =
  Type.iter_variables
    (fun found ->
      if Variable.equal Typed.Type.equal variable found then begin
        let naming = Message.naming () in
        Message.fail "Occurs check failed for %s in %s"
          (Message.within naming (TVar variable))
          (Message.within naming ty)
      end
      else Variable.lower_to ~from:variable found)
    ty

let bind variable ty =
  admit variable ty;
  Option.iter
    (fun required -> satisfy required ty)
    (Variable.constraint_of variable);
  Variable.link variable ty

let merge one other =
  Option.iter (Variable.constrain other) (constraint_of_both one other);
  Variable.lower_to ~from:one other;
  Variable.link one (TVar other)

let rec rewrite_row row label =
  match Type.head row with
  | TRowEmpty -> Message.fail "label %s cannot be inserted" label
  | TRowExtend (found, ty, tail) when String.equal found label -> (ty, tail)
  | TRowExtend (found, ty, tail) -> begin
      match Type.head tail with
      | TVar variable ->
          let field = fresh_variable None in
          let rest = fresh_variable None in
          bind variable (TRowExtend (label, field, rest));
          (field, TRowExtend (found, ty, rest))
      | rest_of_the_row ->
          let field, rest = rewrite_row rest_of_the_row label in
          (field, TRowExtend (found, ty, rest))
    end
  | TVar _ | TInt | TFloat | TChar | TBool | TStr | TUnit | TFun _ | TTup _
  | TCustom _ | TRecord _ ->
      Message.fail "%s is not a record and has no field %s" (Message.of_type row)
        label

let rec tail_of row =
  match Type.head row with
  | TRowExtend (_, _, rest) -> tail_of rest
  | ( TVar _ | TInt | TFloat | TChar | TBool | TStr | TUnit | TFun _ | TTup _
    | TCustom _ | TRecord _ | TRowEmpty ) as tail ->
      tail

let still_open tail =
  match tail with
  | TVar variable -> begin
      match Variable.state variable with
      | Variable.Linked _ -> Message.fail "recursive row type"
      | Variable.Unbound _ -> ()
    end
  | TInt | TFloat | TChar | TBool | TStr | TUnit | TFun _ | TTup _ | TCustom _
  | TRecord _ | TRowExtend _ | TRowEmpty ->
      ()

let rec types left right =
  let mismatch () =
    let naming = Message.naming () in
    Message.fail "Unification failed for %s and %s"
      (Message.within naming left) (Message.within naming right)
  in
  let in_order these those =
    if List.length these <> List.length those then mismatch ()
    else List.iter2 types these those
  in
  match (Type.head left, Type.head right) with
  | TVar one, TVar other when Variable.equal Typed.Type.equal one other -> ()
  | TVar one, TVar other -> merge one other
  | TVar variable, ty | ty, TVar variable -> bind variable ty
  | TInt, TInt
  | TFloat, TFloat
  | TChar, TChar
  | TStr, TStr
  | TBool, TBool
  | TRowEmpty, TRowEmpty
  | TUnit, TUnit ->
      ()
  | TFun (parameter, result), TFun (other_parameter, other_result) ->
      types parameter other_parameter;
      types result other_result
  | TTup items, TTup other_items -> in_order items other_items
  | TCustom (name, arguments), TCustom (other_name, other_arguments) ->
      if Data.Name.equal name other_name then in_order arguments other_arguments
      else mismatch ()
  | TRecord row, TRecord other_row -> types row other_row
  | TRowEmpty, TRowExtend (label, _, _) ->
      Message.fail "Extra field '%s' in record" label
  | TRowExtend (label, _, _), TRowEmpty ->
      Message.fail "Missing field '%s' in record" label
  | TRowExtend (label, field, rest), (TRowExtend _ as other_row) ->
      let tail = tail_of rest in
      let other_field, other_rest = rewrite_row other_row label in
      still_open tail;
      types field other_field;
      types rest other_rest
  | ( ( TInt | TFloat | TChar | TStr | TBool | TUnit | TFun _ | TTup _
      | TCustom _ | TRecord _ | TRowExtend _ | TRowEmpty ),
      _ ) ->
      mismatch ()
