open Typed
open Typed.Type
module Error = Reporting.Error
module Problem = Reporting.Type_error
module Expectation = Reporting.Expectation

exception Cannot
exception Loops of Typed.Type.t

let list_element_of name arguments =
  match arguments with
  | [ element ] when Data.Name.equal name (Data.Name.local "List") ->
      Some element
  | _ -> None

let combine left right =
  match Data.Constraint.combined left right with
  | Some together -> together
  | None -> raise Cannot

let narrow required variable =
  match Variable.constraint_of variable with
  | None -> Variable.constrain variable required
  | Some carried ->
      let together = combine carried required in
      if not (Data.Constraint.equal together carried) then
        Variable.constrain variable together

let rec satisfy (required : Data.Constraint.t) ty =
  match (required, Type.head ty) with
  | _, TVar variable -> narrow required variable
  | Number, (TInt | TFloat) -> ()
  | Comparable, (TInt | TFloat | TChar | TStr) -> ()
  | (Appendable | Comp_appendable), TStr -> ()
  | Comparable, TTup items -> List.iter (satisfy Comparable) items
  | ( (Number | Appendable | Comparable | Comp_appendable),
      TCustom (name, arguments) ) -> begin
      match (required, list_element_of name arguments) with
      | _, None -> raise Cannot
      | Appendable, Some _ -> ()
      | (Comparable | Comp_appendable), Some element -> satisfy Comparable element
      | Number, Some _ -> raise Cannot
    end
  | ( (Number | Comparable | Appendable | Comp_appendable),
      ( TInt | TFloat | TChar | TStr | TBool | TUnit | TFun _ | TTup _
      | TRecord _ | TRowExtend _ | TRowEmpty ) ) ->
      raise Cannot

let constraint_of_both one other =
  match (Variable.constraint_of one, Variable.constraint_of other) with
  | None, carried -> carried
  | carried, None -> carried
  | Some carried, Some carried_other -> Some (combine carried carried_other)

let admit variable ty =
  Type.iter_variables
    (fun found ->
      if Variable.equal Typed.Type.equal variable found then raise (Loops ty)
      else Variable.lower_to ~from:variable found)
    ty

let bind variable ty =
  admit variable ty;
  Option.iter (fun required -> satisfy required ty) (Variable.constraint_of variable);
  Variable.link variable ty

let merge one other =
  Option.iter (Variable.constrain other) (constraint_of_both one other);
  Variable.lower_to ~from:one other;
  Variable.link one (TVar other)

let rec rewrite_row row label =
  match Type.head row with
  | TRowEmpty -> raise Cannot
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
      raise Cannot

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
      | Variable.Linked _ -> raise Cannot
      | Variable.Unbound _ -> ()
    end
  | TInt | TFloat | TChar | TBool | TStr | TUnit | TFun _ | TTup _ | TCustom _
  | TRecord _ | TRowExtend _ | TRowEmpty ->
      ()

let rec fit left right =
  let in_order these those =
    if List.length these <> List.length those then raise Cannot
    else List.iter2 fit these those
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
      fit parameter other_parameter;
      fit result other_result
  | TTup items, TTup other_items -> in_order items other_items
  | TCustom (name, arguments), TCustom (other_name, other_arguments) ->
      if Data.Name.equal name other_name then in_order arguments other_arguments
      else raise Cannot
  | TRecord row, TRecord other_row -> fit row other_row
  | TRowEmpty, TRowExtend _ | TRowExtend _, TRowEmpty -> raise Cannot
  | TRowExtend (label, field, rest), (TRowExtend _ as other_row) ->
      let tail = tail_of rest in
      let other_field, other_rest = rewrite_row other_row label in
      still_open tail;
      fit field other_field;
      fit rest other_rest
  | ( ( TInt | TFloat | TChar | TStr | TBool | TUnit | TFun _ | TTup _
      | TCustom _ | TRecord _ | TRowExtend _ | TRowEmpty ),
      _ ) ->
      raise Cannot

let types ~region ~category ~expected found =
  try fit found (Expectation.expected_type expected) with
  | Cannot ->
      Error.raise_type ~region (Problem.Bad_expression { category; found; expected })
  | Loops looping ->
      Error.raise_type ~region (Problem.Infinite_type { category; found = looping })

let pattern ~region ~category ~expected found =
  try fit found (Expectation.expected_pattern_type expected) with
  | Cannot ->
      Error.raise_type ~region (Problem.Bad_pattern { category; found; expected })
  | Loops looping ->
      Error.raise_type ~region
        (Problem.Infinite_type
           { category = Reporting.Category.Record; found = looping })
