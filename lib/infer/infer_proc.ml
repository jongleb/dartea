open Typed
open Typed.Type
module Name_map = Data.Name.Map

let fail format = Printf.ksprintf failwith format

type ctx = scheme Name_map.t

type type_env = {
  types : Canonical.Typedecl.t Name_map.t;
  constructors :
    (Canonical.Typedecl.t * Canonical.Typedecl.type_ctor) Name_map.t;
  aliases : (string list * Typed.Type.t) Name_map.t;
}

type ctor_info = { name : Data.Name.t; arity : int; index : int; total : int }

type infer_result = {
  ctx : ctx;
  declarations : Typed.Declaration.t list;
  siblings_env : (Data.Name.t * int) list Name_map.t;
  constructors : ctor_info list;
  typedecls : Canonical.Typedecl.t list;
}

let primitive_ctx : ctx =
  List.fold_left
    (fun acc (name, scheme) -> Name_map.add (Data.Name.local name) scheme acc)
    Name_map.empty Primitives.values

open Canonical.Expr
module Str_set = Set.Make (String)

let rec ftv_typ = function
  | TVar v -> Str_set.singleton v
  | TInt | TFloat | TChar | TBool | TStr | TUnit | TRowEmpty -> Str_set.empty
  | TFun (p, r) -> Str_set.union (ftv_typ p) (ftv_typ r)
  | TTup l ->
      List.fold_left
        (fun acc ty -> Str_set.union (ftv_typ ty) acc)
        Str_set.empty l
  | TCustom (_, typs) ->
      List.fold_left
        (fun acc ty -> Str_set.union (ftv_typ ty) acc)
        Str_set.empty typs
  | TRecord typ -> ftv_typ typ
  | TRowExtend (_l, t, r) -> Str_set.union (ftv_typ r) (ftv_typ t)


module Subst = struct
  include Typed.Type.By_variable

  type nonrec t = Typed.Type.t t

  let apply ty s = Typed.Type.substitute s ty

  let compose outer inner =
    map (fun ty -> apply ty outer) inner |> union (fun _ later _ -> Some later) outer

  let to_scheme scheme s =
    match scheme with
    | Scheme (quantified, ty) ->
        Scheme (quantified, apply ty (List.fold_right remove quantified s))

  let to_ctx ctx s = Name_map.map (fun scheme -> to_scheme scheme s) ctx
end

let ( ++ ) = Subst.compose

let rec substitute_expr (e : Typed.Expr.t) s =
  let open Typed.Expr in
  let sub e = substitute_expr e s in
  let expr =
    match e.expr with
    | Expr_constr constr ->
        Expr_constr { constr with arguments = List.map sub constr.arguments }
    | Expr_binop binop ->
        let left, right = binop.operands in
        Expr_binop { binop with operands = (sub left, sub right) }
    | Expr_let { binding = { bind_body = { name; body } }; body = rest } ->
        Expr_let
          {
            binding = { bind_body = { name; body = sub body } };
            body = sub rest;
          }
    | Expr_if_then_else { if_exp; then_exp; else_exp } ->
        Expr_if_then_else
          {
            if_exp = sub if_exp;
            then_exp = sub then_exp;
            else_exp = sub else_exp;
          }
    | Expr_record rows ->
        Expr_record
          (List.map (fun row -> { row with value = sub row.value }) rows)
    | Expr_apply { fn; arg } -> Expr_apply { fn = sub fn; arg = sub arg }
    | Expr_pattern { expr; pattern_data_items } ->
        Expr_pattern
          {
            expr = sub expr;
            pattern_data_items =
              List.map
                (fun (case : expr_pattern_case) ->
                  {
                    pattern = Typed.Pattern.substitute s case.pattern;
                    expr = sub case.expr;
                  })
                pattern_data_items;
          }
    | Expr_access { expr; field } -> Expr_access { expr = sub expr; field }
    | Expr_lambda { params; body } ->
        Expr_lambda
          {
            params =
              List.map
                (fun (param : expr_lambda_param) ->
                  { param with typ = Subst.apply param.typ s })
                params;
            body = sub body;
          }
    | Expr_list items -> Expr_list (List.map sub items)
    | Expr_cons { head; tail } -> Expr_cons { head = sub head; tail = sub tail }
    | Expr_tuple items -> Expr_tuple (List.map sub items)
    | Expr_record_update { record; fields } ->
        Expr_record_update
          {
            record = sub record;
            fields =
              List.map
                (fun (row : expr_record_row) ->
                  { row with value = sub row.value })
                fields;
          }
    | ( Expr_ident _ | Expr_accessor _ | Expr_record_extend _
      | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_kernel _
      | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _ ) as leaf ->
        leaf
  in
  { typ = Subst.apply e.typ s; expr }

let substitute_declaration (decl : Typed.Declaration.t) s =
  let parameter (param : Typed.Declaration.param) =
    { param with typ = Subst.apply param.typ s }
  in
  {
    decl with
    params = List.map parameter decl.params;
    body = substitute_expr decl.body s;
    typ = Subst.apply decl.typ s;
  }

let string_of_typ ty =
  let rec written ty =
    match ty with
    | TVar v -> v
    | TInt -> "Int"
    | TFloat -> "Float"
    | TChar -> "Char"
    | TBool -> "Bool"
    | TUnit -> "()"
    | TStr -> "String"
    | TFun (parameter, result) ->
        parenthesised parameter ^ " -> " ^ written result
    | TTup items -> "( " ^ String.concat ", " (List.map written items) ^ " )"
    | TCustom (name, []) -> Data.Name.to_string name
    | TCustom (name, arguments) ->
        Data.Name.to_string name ^ " "
        ^ String.concat " " (List.map parenthesised arguments)
    | TRecord row -> "{ " ^ String.concat ", " (fields row) ^ " }"
    | TRowExtend _ | TRowEmpty -> "{ " ^ String.concat ", " (fields ty) ^ " }"
  and fields row =
    match row with
    | TRowEmpty -> []
    | TVar v -> [ v ]
    | TRowExtend (label, typ, rest) ->
        Printf.sprintf "%s : %s" label (written typ) :: fields rest
    | TInt | TFloat | TChar | TBool | TStr | TUnit | TFun _ | TTup _
    | TCustom _ | TRecord _ ->
        [ written row ]
  and parenthesised ty =
    match ty with
    | TFun _ | TCustom (_, _ :: _) -> "(" ^ written ty ^ ")"
    | TVar _ | TInt | TFloat | TChar | TBool | TStr | TUnit | TTup _
    | TCustom (_, []) | TRecord _ | TRowExtend _ | TRowEmpty ->
        written ty
  in
  written ty

let ftv_scheme = function
  | Scheme (vars, ty) -> Str_set.diff (ftv_typ ty) (Str_set.of_list vars)

let ftv_ctx ctx =
  Name_map.fold
    (fun _ scheme acc -> Str_set.union acc (ftv_scheme scheme))
    ctx Str_set.empty

let generalize ty ctx =
  let vars = Str_set.diff (ftv_typ ty) (ftv_ctx ctx) |> Str_set.to_seq in
  Scheme (List.of_seq vars, ty)

module Fresh = struct
  let taken = ref 0
  let reset () = taken := 0

  let var prefix =
    let id = !taken in
    incr taken;
    TVar (Printf.sprintf "%s%c%i" prefix Data.Constraint.generated id)

  let any () = var "a"
  let row () = var "r"
  let number () = var (Data.Constraint.name Data.Constraint.Number)
end

let list_element_of name arguments =
  match arguments with
  | [ element ] when Data.Name.equal name (Data.Name.local "List") ->
      Some element
  | _ -> None

let combining left right =
  match Data.Constraint.combined left right with
  | Some together -> together
  | None ->
      fail "%s and %s cannot be the same type variable"
        (Data.Constraint.name left) (Data.Constraint.name right)

let narrowed required variable =
  let renamed together =
    Subst.singleton variable (Fresh.var (Data.Constraint.name together))
  in
  match Data.Constraint.of_variable variable with
  | None -> renamed required
  | Some carried ->
      let together = combining carried required in
      if together = carried then Subst.empty else renamed together

let rec satisfying (required : Data.Constraint.t) ty =
  let unsatisfied () =
    fail "%s does not satisfy %s" (string_of_typ ty)
      (Data.Constraint.name required)
  in
  let every constrained items =
    List.fold_left
      (fun acc item -> satisfying constrained (Subst.apply item acc) ++ acc)
      Subst.empty items
  in
  match (required, ty) with
  | _, TVar variable -> narrowed required variable
  | Number, (TInt | TFloat) -> Subst.empty
  | Comparable, (TInt | TFloat | TChar | TStr) -> Subst.empty
  | (Appendable | Comp_appendable), TStr -> Subst.empty
  | Comparable, TTup items -> every Comparable items
  | ( (Number | Appendable | Comparable | Comp_appendable),
      TCustom (name, arguments) ) -> begin
      match (required, list_element_of name arguments) with
      | _, None -> unsatisfied ()
      | Appendable, Some _ -> Subst.empty
      | (Comparable | Comp_appendable), Some element ->
          satisfying Comparable element
      | Number, Some _ -> unsatisfied ()
    end
  | ( (Number | Comparable | Appendable | Comp_appendable),
      ( TInt | TFloat | TChar | TStr | TBool | TUnit | TFun _ | TTup _
      | TRecord _ | TRowExtend _ | TRowEmpty ) ) ->
      unsatisfied ()

let unify_variables left right =
  if String.equal left right then Subst.empty
  else
    match
      (Data.Constraint.of_variable left, Data.Constraint.of_variable right)
    with
    | None, _ -> Subst.singleton left (TVar right)
    | Some _, None -> Subst.singleton right (TVar left)
    | Some carried_left, Some carried_right ->
        let together = combining carried_left carried_right in
        if together = carried_left then Subst.singleton right (TVar left)
        else if together = carried_right then Subst.singleton left (TVar right)
        else
          let fresh = Fresh.var (Data.Constraint.name together) in
          Subst.add right fresh (Subst.singleton left fresh)

let bind_var ty v =
  match ty with
  | TVar v' when String.equal v v' -> Subst.empty
  | _ ->
      if Str_set.mem v (ftv_typ ty) then
        fail "Occurs check failed for %s in %s" v (string_of_typ ty)
      else begin
        match Data.Constraint.of_variable v with
        | None -> Subst.singleton v ty
        | Some required ->
            let narrowing = satisfying required ty in
            Subst.singleton v (Subst.apply ty narrowing) ++ narrowing
      end

let rec rewrite_row row label =
  match row with
  | TRowEmpty -> fail "label %s cannot be inserted" label
  | TRowExtend (label2, ty, tail) when label = label2 -> (ty, tail, Subst.empty)
  | TRowExtend (label2, ty, tail) -> begin
      match tail with
      | TVar a ->
          let new_r = Fresh.row () in
          let new_a = Fresh.any () in
          ( new_a,
            TRowExtend (label2, ty, new_r),
            Subst.singleton a (TRowExtend (label, new_a, new_r)) )
      | _ ->
          let ty2, tail2, s = rewrite_row tail label in
          (ty2, TRowExtend (label2, ty, tail2), s)
    end
  | TVar _ | TInt | TFloat | TChar | TBool | TStr | TUnit | TFun _ | TTup _
  | TCustom _ | TRecord _ ->
      fail "%s is not a record and has no field %s" (string_of_typ row) label

let rec unify ty1 ty2 =
  let unify_err ty1 ty2 =
    let ty1' = string_of_typ ty1 and ty2' = string_of_typ ty2 in
    fail "Unification failed for %s and %s" ty1' ty2'
  in
  let rec unify_in_order left right =
    List.fold_left2
      (fun acc ty1 ty2 -> unify' (Subst.apply ty1 acc, Subst.apply ty2 acc) ++ acc)
      Subst.empty left right

  and unify' = function
    | TVar left, TVar right -> unify_variables left right
    | TVar v, ty | ty, TVar v -> bind_var ty v
    | TInt, TInt
    | TFloat, TFloat
    | TChar, TChar
    | TStr, TStr
    | TBool, TBool
    | TRowEmpty, TRowEmpty
    | TUnit, TUnit ->
        Subst.empty
    | TFun (p, r), TFun (p', r') ->
        let s1 = unify' (p, p') in
        let s2 = unify' (Subst.apply r s1, Subst.apply r' s1) in
        s2 ++ s1
    | TTup l, TTup l' ->
        if List.length l != List.length l' then unify_err ty1 ty2
        else unify_in_order l l'
    | TCustom (name1, args1), TCustom (name2, args2) ->
        if Data.Name.equal name1 name2 then unify_in_order args1 args2
        else unify_err ty1 ty2
    | TRecord ty1, TRecord ty2 -> unify' (ty1, ty2)
    | TRowEmpty, TRowExtend (label, _, _) ->
        fail "Extra field '%s' in record" label
    | TRowExtend (label, _, _), TRowEmpty ->
        fail "Missing field '%s' in record" label
    | TRowExtend (l1, ty1, rt1), (TRowExtend (_, _, _) as row2) -> begin
        let ty2, rt2, s1 = rewrite_row row2 l1 in
        let rec to_list ty =
          match ty with
          | TVar name -> ([], Some name)
          | TRowEmpty -> ([], None)
          | TRowExtend (l, t, r) ->
              let ls, mv = to_list r in
              ((l, t) :: ls, mv)
          | TInt | TFloat | TChar | TBool | TStr | TUnit | TFun _ | TTup _
          | TCustom _ | TRecord _ ->
              fail "%s cannot end a record" (string_of_typ ty)
        in
        let result = to_list rt1 in
        match snd result with
        | Some tv when Subst.mem tv s1 -> failwith "recursive row type"
        | _ ->
            let s2 = unify' (Subst.apply ty1 s1, Subst.apply ty2 s1) in
            let s3 = s2 ++ s1 in
            let s4 = unify' (Subst.apply rt1 s3, Subst.apply rt2 s3) in
            s4 ++ s3
      end
    | _ -> unify_err ty1 ty2
  in
  unify' (ty1, ty2)

let variable_prefix variable =
  match Data.Constraint.of_variable variable with
  | Some carried -> Data.Constraint.name carried
  | None -> "a"

let instantiate = function
  | Scheme (vars, ty) ->
      let nvars = List.map (fun v -> Fresh.var (variable_prefix v)) vars in
      let s =
        List.fold_left2 (fun acc v nv -> Subst.add v nv acc) Subst.empty vars nvars
      in
      (s, Subst.apply ty s)

let bindings_of_both here there =
  Name_map.union
    (fun name _ _ ->
      fail "%s is bound twice in one pattern" (Data.Name.to_string name))
    here there

let list_of element = TCustom (Data.Name.local "List", [ element ])

let concrete_type name args =
  match (name, args) with
  | Data.Name.Local "Int", [] -> TInt
  | Data.Name.Local "Float", [] -> TFloat
  | Data.Name.Local "Char", [] -> TChar
  | Data.Name.Local "Bool", [] -> TBool
  | Data.Name.Local "String", [] -> TStr
  | Data.Name.Local "Unit", [] -> TUnit
  | _ -> TCustom (name, args)

let typedef_to_type (impl : Canonical.Typedef.Impl.t) =
  let open Canonical.Typedef in
  let rec conv (i : Impl.t) : Type.t =
    let args = List.map conv i.parameters in
    match i.body with
    | Kind.Tkind_var v -> TVar v.thing
    | Kind.Tkind_concrete c -> concrete_type c.thing args
    | Kind.Tkind_tuple types -> TTup (List.map conv types)
    | Kind.Tkind_function fn -> begin
        match List.rev fn.arguments with
        | [] -> failwith "Empty function type"
        | return_impl :: rev_params ->
            function_of (List.rev_map conv rev_params)
              ~result:(conv return_impl)
      end
    | Kind.Tkind_unit -> TUnit
    | Kind.Tkind_record fields ->
        let base =
          match fields.row_type with
          | Some row_var -> TVar row_var.thing
          | None -> TRowEmpty
        in
        let row_type =
          List.fold_right
            (fun (row : Type_record_row.t) acc ->
              TRowExtend (row.name.thing, conv row.body, acc))
            fields.values base
        in
        TRecord row_type
  in
  conv impl

let constructor_scheme (typedef : Canonical.Typedecl.t)
    (ctor : Canonical.Typedecl.type_ctor) =
  let result_type =
    concrete_type typedef.name (List.map (fun p -> TVar p) typedef.params)
  in
  Scheme
    ( typedef.params,
      function_of (List.map typedef_to_type ctor.data) ~result:result_type )

let build_initial_ctx (type_env : type_env) : ctx =
  Name_map.fold
    (fun ctor_name (typedef, ctor) acc ->
      Name_map.add ctor_name (constructor_scheme typedef ctor) acc)
    type_env.constructors primitive_ctx

let build_type_env ~(imports : Interface.t list)
    (module_ : Canonical.Module.t) : type_env =
  let declared_here =
    Canonical.Module.String_map.fold
      (fun _ typedecl acc -> typedecl :: acc)
      module_.type_declarations []
  in
  let imported = List.concat_map (fun (i : Interface.t) -> i.types) imports in
  let visible = Primitives.types @ imported @ declared_here in
  let add_type acc (typedecl : Canonical.Typedecl.t) =
    Name_map.add typedecl.name typedecl acc
  in
  let add_constructors acc (typedecl : Canonical.Typedecl.t) =
    List.fold_left
      (fun acc (ctor : Canonical.Typedecl.type_ctor) ->
        Name_map.add ctor.id (typedecl, ctor) acc)
      acc typedecl.ctors
  in
  let add_alias name (alias : Canonical.Typealias.t) acc =
    Name_map.add name (alias.params, typedef_to_type alias.typedef) acc
  in
  let imported_aliases =
    List.concat_map (fun (i : Interface.t) -> i.type_aliases) imports
    |> List.fold_left
         (fun acc (alias : Canonical.Typealias.t) ->
           add_alias alias.name alias acc)
         Name_map.empty
  in
  let aliases =
    Canonical.Module.String_map.fold
      (fun name alias acc -> add_alias (Data.Name.local name) alias acc)
      module_.type_aliases imported_aliases
  in
  {
    types = List.fold_left add_type Name_map.empty visible;
    constructors = List.fold_left add_constructors Name_map.empty visible;
    aliases;
  }

let rec expand_type_alias type_env ty =
  let expand = expand_type_alias type_env in
  match ty with
  | TCustom (name, args) -> begin
      match Name_map.find_opt name type_env.aliases with
      | None -> TCustom (name, List.map expand args)
      | Some (params, alias_body)
        when List.length params = List.length args ->
          let substitution =
            List.fold_left2
              (fun acc param arg -> Subst.add param (expand arg) acc)
              Subst.empty params args
          in
          expand (Subst.apply alias_body substitution)
      | Some (params, _) ->
          fail "Type alias %s expects %d arguments, got %d"
            (Data.Name.to_string name) (List.length params) (List.length args)
    end
  | TFun (p, r) -> TFun (expand p, expand r)
  | TTup l -> TTup (List.map expand l)
  | TRecord t -> TRecord (expand t)
  | TRowExtend (label, t, r) -> TRowExtend (label, expand t, expand r)
  | t -> t

let rec infer_pattern (type_env : type_env) (pattern : Canonical.Pattern.t) :
    Subst.t * Typed.Pattern.t * ctx =
  let go = infer_pattern type_env in
  let in_order patterns =
    let learned, reversed, bound =
      List.fold_left
        (fun (learned, inferred, bound) pattern ->
          let substitution, typed, bound_here = go pattern in
          ( substitution ++ learned,
            typed :: inferred,
            bindings_of_both bound bound_here ))
        (Subst.empty, [], Name_map.empty)
        patterns
    in
    (learned, List.rev reversed, bound)
  in
  match pattern with
  | Canonical.Pattern.P_var name ->
      let bound_type = Fresh.any () in
      ( Subst.empty,
        { Pattern.typ = bound_type; pattern = P_T_var name },
        Name_map.singleton (Data.Name.local name) (Scheme ([], bound_type)) )
  | P_anything ->
      (Subst.empty, { typ = Fresh.any (); pattern = P_T_anything }, Name_map.empty)
  | P_int value ->
      ( Subst.empty,
        { typ = Fresh.number (); pattern = P_T_int value },
        Name_map.empty )
  | P_str text ->
      (Subst.empty, { typ = TStr; pattern = P_T_str text }, Name_map.empty)
  | P_chr letter ->
      (Subst.empty, { typ = TChar; pattern = P_T_chr letter }, Name_map.empty)
  | P_unit -> (Subst.empty, { typ = TUnit; pattern = P_T_unit }, Name_map.empty)
  | P_tuple items ->
      let learned, inferred, bound = in_order items in
      ( learned,
        {
          typ = TTup (List.map (fun typed -> typed.Pattern.typ) inferred);
          pattern = P_T_tuple inferred;
        },
        bound )
  | P_list items ->
      let element = Fresh.any () in
      let walked, inferred, bound = in_order items in
      let learned =
        List.fold_left
          (fun learned typed ->
            unify
              (Subst.apply element learned)
              (Subst.apply typed.Pattern.typ learned)
            ++ learned)
          walked inferred
      in
      ( learned,
        {
          typ = list_of (Subst.apply element learned);
          pattern = P_T_list (List.map (Typed.Pattern.substitute learned) inferred);
        },
        bound )
  | P_alias (inner, name) ->
      let learned, aliased, bound = go inner in
      let typ = aliased.Pattern.typ in
      ( learned,
        { typ; pattern = P_T_alias (aliased, name) },
        Name_map.add (Data.Name.local name) (Scheme ([], typ)) bound )
  | P_cons (head, tail) ->
      let head_substitution, typed_head, head_bound = go head in
      let tail_substitution, typed_tail, tail_bound = go tail in
      let list_type = list_of typed_head.Pattern.typ in
      let tail_is_a_list =
        unify (Subst.apply typed_tail.typ tail_substitution) list_type
      in
      let learned = tail_is_a_list ++ tail_substitution ++ head_substitution in
      ( learned,
        {
          typ = Subst.apply list_type learned;
          pattern = P_T_cons (typed_head, typed_tail);
        },
        bindings_of_both head_bound tail_bound )
  | P_ctor (name, arguments) -> begin
      match Name_map.find_opt name type_env.constructors with
      | None ->
          fail "Unknown constructor %s" (Data.Name.to_string name)
      | Some (declared, ctor) ->
          let wrong_arity () =
            let takes = List.length ctor.data in
            fail "Constructor %s takes %d argument%s, the pattern gives %d"
              (Data.Name.to_string name) takes
              (if takes = 1 then "" else "s")
              (List.length arguments)
          in
          let rec against_payloads (learned, inferred, bound) arguments carried =
            match (arguments, carried) with
            | argument :: rest, TFun (payload, remaining) ->
                let substitution, typed, bound_here = go argument in
                let learned = substitution ++ learned in
                let carries_the_payload =
                  unify
                    (Subst.apply typed.Pattern.typ learned)
                    (Subst.apply payload learned)
                in
                against_payloads
                  ( carries_the_payload ++ learned,
                    typed :: inferred,
                    bindings_of_both bound bound_here )
                  rest remaining
            | [], TFun _ | _ :: _, _ -> wrong_arity ()
            | [], matched -> (learned, inferred, bound, matched)
          in
          let learned, inferred, bound, matched =
            against_payloads
              (Subst.empty, [], Name_map.empty)
              arguments
              (snd (instantiate (constructor_scheme declared ctor)))
          in
          ( learned,
            {
              typ = Subst.apply matched learned;
              pattern =
                P_T_ctor
                  ( name,
                    List.rev_map (Typed.Pattern.substitute learned) inferred );
            },
            Subst.to_ctx bound learned )
    end
  | P_record fields ->
      let row_var = Fresh.row () in
      let row, bound =
        List.fold_right
          (fun field (row, bound) ->
            let field_type = Fresh.any () in
            ( TRowExtend (field, field_type, row),
              Name_map.add (Data.Name.local field) (Scheme ([], field_type)) bound ))
          fields (row_var, Name_map.empty)
      in
      (Subst.empty, { typ = TRecord row; pattern = P_T_record fields }, bound)

module Infer = struct
  type 'a t = Subst.t -> Subst.t * 'a

  let return value learned = (learned, value)

  let ( let* ) computation f learned =
    let learned, value = computation learned in
    f value learned

  let ( let+ ) computation f learned =
    let learned, value = computation learned in
    (learned, f value)

  let run computation = computation Subst.empty
  let knowing substitution learned = (substitution ++ learned, ())
  let resolve ty learned = (learned, Subst.apply ty learned)
  let resolve_ctx ctx learned = (learned, Subst.to_ctx ctx learned)

  let unified left right learned =
    (unify (Subst.apply left learned) (Subst.apply right learned) ++ learned, ())

  let rec traverse f = function
    | [] -> return []
    | item :: rest ->
        let* first = f item in
        let+ others = traverse f rest in
        first :: others
end

let assume_parameters ctx params =
  let assumed = List.map (fun _ -> Fresh.any ()) params in
  let visible =
    List.fold_left2
      (fun visible param typ ->
        Name_map.add
          (Data.Name.local param.Data.Located.thing)
          (Scheme ([], typ))
          visible)
      ctx params assumed
  in
  (assumed, visible)

let matching_the_annotation type_env subject = function
  | None -> Infer.return ()
  | Some written ->
      Infer.unified subject (expand_type_alias type_env (typedef_to_type written))

type matched_so_far = {
  scrutinee_type : Type.t;
  result_type : Type.t;
  branches : Typed.Expr.expr_pattern_case list;
}

let rec infer_with_env (exp : Canonical.Expr.t) ctx (type_env : type_env) :
    Typed.Expr.t Infer.t =
  let open Infer in
  let infer exp ctx learned = infer_with_env exp (Subst.to_ctx ctx learned) type_env learned in
  let go exp = infer exp ctx in
  let node expr typ = { Typed.Expr.expr; typ } in
  match exp with
  | Expr_int value -> return (node (Typed.Expr.Expr_int value) (Fresh.number ()))
  | Expr_float value -> return (node (Expr_float value) TFloat)
  | Expr_string text -> return (node (Expr_string text) TStr)
  | Expr_char letter -> return (node (Expr_char letter) TChar)
  | Expr_unit -> return (node Typed.Expr.Expr_unit TUnit)
  | Expr_kernel primitive ->
      return (node (Typed.Expr.Expr_kernel primitive) (Fresh.any ()))
  | Expr_ident name -> begin
      match Name_map.find_opt name ctx with
      | None -> fail "Unbound value %s" (Data.Name.to_string name)
      | Some scheme -> return (node (Expr_ident name) (snd (instantiate scheme)))
    end
  | Expr_apply { fn; arg } ->
      let* typed_callee = go fn in
      let* typed_argument = go arg in
      let result = Fresh.any () in
      let* () = unified typed_callee.typ (TFun (typed_argument.typ, result)) in
      let+ typ = resolve result in
      node (Expr_apply { fn = typed_callee; arg = typed_argument }) typ
  | Expr_if_then_else { if_exp; then_exp; else_exp } ->
      let* typed_condition = go if_exp in
      let* () = unified typed_condition.typ TBool in
      let* typed_then = go then_exp in
      let* typed_else = go else_exp in
      let* () = unified typed_then.typ typed_else.typ in
      let+ typ = resolve typed_else.typ in
      node
        (Expr_if_then_else
           {
             if_exp = typed_condition;
             then_exp = typed_then;
             else_exp = typed_else;
           })
        typ
  | Expr_list items ->
      let element = Fresh.any () in
      let* inferred =
        traverse
          (fun item ->
            let* typed = go item in
            let+ () = unified element typed.Typed.Expr.typ in
            typed)
          items
      in
      let+ settled = resolve element in
      node (Expr_list inferred) (list_of settled)
  | Expr_record_update { record; fields } ->
      let* typed_record = go record in
      let+ inferred =
        traverse
          (fun (row : Canonical.Expr.expr_record_row) ->
            let* typed_value = go row.value in
            let others = Fresh.row () in
            let+ () =
              unified typed_record.typ
                (TRecord (TRowExtend (row.name, typed_value.typ, others)))
            in
            { Typed.Expr.name = row.name; value = typed_value })
          fields
      in
      node
        (Expr_record_update { record = typed_record; fields = inferred })
        typed_record.typ
  | Expr_tuple items ->
      let+ inferred = traverse go items in
      node (Expr_tuple inferred)
        (TTup (List.map (fun (item : Typed.Expr.t) -> item.typ) inferred))
  | Expr_cons { head; tail } ->
      let* typed_head = go head in
      let* typed_tail = go tail in
      let* () = unified typed_tail.typ (list_of typed_head.typ) in
      let+ typ = resolve typed_tail.typ in
      node (Expr_cons { head = typed_head; tail = typed_tail }) typ
  | Expr_let { binding = { bind_type; bind_body = { name; body = rhs } }; body }
    ->
      let assumed = Fresh.any () in
      let visible_to_itself =
        Name_map.add (Data.Name.local name.thing) (Scheme ([], assumed)) ctx
      in
      let* typed_bound = infer rhs visible_to_itself in
      let* () = unified assumed typed_bound.typ in
      let* () =
        matching_the_annotation type_env typed_bound.typ
          (Option.map (fun annotation -> annotation.content) bind_type)
      in
      let* bound_type = resolve typed_bound.typ in
      let* outside = resolve_ctx ctx in
      let typed_bound = { typed_bound with Typed.Expr.typ = bound_type } in
      let visible_to_the_body =
        Name_map.add
          (Data.Name.local name.thing)
          (generalize bound_type outside)
          outside
      in
      let+ typed_body = infer body visible_to_the_body in
      node
        (Expr_let
           {
             binding = { bind_body = { name; body = typed_bound } };
             body = typed_body;
           })
        typed_body.typ
  | Expr_pattern { expr; pattern_data_items } -> begin
      let branch matched { pattern; expr = branch_expr } =
        let pattern_substitution, typed_pattern, bound =
          infer_pattern type_env pattern
        in
        let* () = knowing pattern_substitution in
        let* () = unified typed_pattern.Pattern.typ matched.scrutinee_type in
        let visible_in_the_branch =
          Name_map.union (fun _ _ inner -> Some inner) ctx bound
        in
        let* typed_branch = infer branch_expr visible_in_the_branch in
        let* () = unified typed_branch.typ matched.result_type in
        let* scrutinee_type = resolve typed_pattern.typ in
        let+ result_type = resolve typed_branch.typ in
        {
          scrutinee_type;
          result_type;
          branches =
            { Typed.Expr.pattern = typed_pattern; expr = typed_branch }
            :: matched.branches;
        }
      in
      match pattern_data_items with
      | [] -> failwith "A case expression needs at least one branch"
      | written ->
          let* scrutinee = go expr in
          let start =
            {
              scrutinee_type = scrutinee.typ;
              result_type = Fresh.any ();
              branches = [];
            }
          in
          let+ matched =
            List.fold_left
              (fun carried item ->
                let* matched = carried in
                branch matched item)
              (return start) written
          in
          node
            (Expr_pattern
               {
                 expr = scrutinee;
                 pattern_data_items = List.rev matched.branches;
               })
            matched.result_type
    end
  | Expr_record_extend label ->
      let a = Fresh.any () in
      let r = Fresh.row () in
      return
        (node (Expr_record_extend label)
           (TFun (a, TFun (TRecord r, TRecord (TRowExtend (label, a, r))))))
  | Expr_record_empty -> return (node Expr_record_empty (TRecord TRowEmpty))
  | Expr_record_select label ->
      let a = Fresh.any () in
      let r = Fresh.row () in
      return
        (node (Expr_record_select label)
           (TFun (TRecord (TRowExtend (label, a, r)), a)))
  | Expr_accessor field ->
      let a = Fresh.any () in
      let r = Fresh.row () in
      let label = Data.Located.unwrap field in
      return
        (node (Expr_accessor field) (TFun (TRecord (TRowExtend (label, a, r)), a)))
  | Expr_access { expr; field } ->
      let* typed_record = go expr in
      let a = Fresh.any () in
      let r = Fresh.row () in
      let* () =
        unified typed_record.typ (TRecord (TRowExtend (field.thing, a, r)))
      in
      let+ typ = resolve a in
      node (Expr_access { expr = typed_record; field }) typ
  | Expr_lambda { params; body } ->
      let assumed, visible_in_the_body = assume_parameters ctx params in
      let* typed_body = infer body visible_in_the_body in
      let+ settled_params = traverse resolve assumed in
      node
        (Expr_lambda
           {
             params =
               List.map2
                 (fun name typ -> { Typed.Expr.name; typ })
                 params settled_params;
             body = typed_body;
           })
        (function_of settled_params ~result:typed_body.typ)

type group_member = {
  position : int;
  declaration : Canonical.Declaration.t;
  member_name : Data.Name.t;
  assumed : Type.t option;
}

type inferred_member = {
  member : group_member;
  typed : Typed.Declaration.t;
  checked : Type.t;
}

let infer_toplevel ~(imports : Interface.t list)
    (module_ : Canonical.Module.t) primitives =
  let type_env = build_type_env ~imports module_ in
  let visible =
    List.concat_map (fun (interface : Interface.t) -> interface.values) imports
    |> List.fold_left
         (fun ctx (value : Interface.value) ->
           Name_map.add value.name value.scheme ctx)
         (Name_map.union
            (fun _ _ declared -> Some declared)
            (build_initial_ctx type_env) primitives)
  in

  let infer_declaration { Canonical.Declaration.body_part; type_part_data } ctx
      =
    let open Infer in
    let assumed, visible_in_the_body = assume_parameters ctx body_part.params in
    run
      (let* typed_body =
         infer_with_env body_part.expr.Data.Located.thing visible_in_the_body
           type_env
       in
       let* settled_params = traverse resolve assumed in
       let* body_type = resolve typed_body.typ in
       let inferred_type = function_of settled_params ~result:body_type in
       let* () =
         matching_the_annotation type_env inferred_type
           (Option.map
              (fun (type_part : Canonical.Declaration.type_part) ->
                type_part.type_alias)
              type_part_data)
       in
       let+ verified_type = resolve inferred_type in
       ( {
           Typed.Declaration.name = body_part.name;
           params =
             List.map2
               (fun name typ -> { Typed.Declaration.name; typ })
               body_part.params settled_params;
           body = typed_body;
           typ = verified_type;
         },
         verified_type ))
  in

  let infer_group ctx group =
    let member (position, (declaration : Canonical.Declaration.t)) =
      let assumed =
        match declaration.type_part_data with
        | Some _ -> None
        | None -> Some (Fresh.any ())
      in
      {
        position;
        declaration;
        member_name =
          Data.Name.local (Data.Located.unwrap declaration.body_part.name);
        assumed;
      }
    in
    let members = List.map member group in
    let inside =
      List.fold_left
        (fun visible member ->
          match member.assumed with
          | None -> visible
          | Some typ ->
              Name_map.add member.member_name (Scheme ([], typ)) visible)
        ctx members
    in
    let infer_member (substitution, inferred) member =
      let member_substitution, (typed, checked) =
        infer_declaration member.declaration (Subst.to_ctx inside substitution)
      in
      let substitution = member_substitution ++ substitution in
      let agreed =
        match member.assumed with
        | None -> substitution
        | Some assumption ->
            unify
              (Subst.apply checked substitution)
              (Subst.apply assumption substitution)
            ++ substitution
      in
      (agreed, { member; typed; checked } :: inferred)
    in
    let substitution, inferred =
      List.fold_left infer_member (Subst.empty, []) members
    in
    let outside = Subst.to_ctx ctx substitution in
    let settled inferred =
      match inferred.member.assumed with
      | None -> inferred.checked
      | Some assumption -> Subst.apply assumption substitution
    in
    let generalized =
      List.fold_left
        (fun visible inferred ->
          Name_map.add inferred.member.member_name
            (generalize (settled inferred) outside)
            visible)
        ctx inferred
    in
    ( generalized,
      List.rev_map
        (fun inferred ->
          ( inferred.member.position,
            substitute_declaration inferred.typed substitution ))
        inferred )
  in

  let announced =
    List.fold_left
      (fun collected (declaration : Canonical.Declaration.t) ->
        match declaration.type_part_data with
        | None -> collected
        | Some annotation ->
            Name_map.add
              (Data.Name.local (Data.Located.unwrap declaration.body_part.name))
              (generalize
                 (expand_type_alias type_env
                    (typedef_to_type annotation.type_alias))
                 Name_map.empty)
              collected)
      visible module_.top_declarations
  in

  let final_ctx, typed_decls =
    List.mapi
      (fun position declaration -> (position, declaration))
      module_.top_declarations
    |> Canonicalization.Declaration_graph.in_dependency_order ~declaration:snd
    |> List.fold_left
         (fun (ctx, collected) group ->
           let ctx, typed = infer_group ctx group in
           (ctx, List.rev_append typed collected))
         (announced, [])
  in

  let as_declared =
    List.sort (fun (left, _) (right, _) -> Int.compare left right) typed_decls
    |> List.map snd
  in

  let payload_arity (ctor : Canonical.Typedecl.type_ctor) =
    List.length ctor.data
  in

  let siblings_env =
    Name_map.to_seq type_env.types
    |> Seq.concat_map (fun (_, (declared : Canonical.Typedecl.t)) ->
           let siblings =
             List.map
               (fun (ctor : Canonical.Typedecl.type_ctor) ->
                 (ctor.id, payload_arity ctor))
               declared.ctors
           in
           List.to_seq declared.ctors
           |> Seq.map (fun (ctor : Canonical.Typedecl.type_ctor) ->
                  (ctor.id, siblings)))
    |> Name_map.of_seq
  in

  let constructors =
    Name_map.fold
      (fun _ (declared : Canonical.Typedecl.t) collected ->
        let total = List.length declared.ctors in
        List.mapi
          (fun index (ctor : Canonical.Typedecl.type_ctor) ->
            { name = ctor.id; arity = payload_arity ctor; index; total })
          declared.ctors
        @ collected)
      type_env.types []
  in

  {
    ctx = final_ctx;
    declarations = as_declared;
    siblings_env;
    constructors;
    typedecls = List.map snd (Name_map.bindings type_env.types);
  }

let interface_of (module_ : Canonical.Module.t) (result : infer_result) :
    Interface.t =
  let values =
    List.fold_left
      (fun acc (d : Canonical.Declaration.t) ->
        let name = Data.Located.unwrap d.body_part.name in
        Name_map.find_opt (Data.Name.local name) result.ctx
        |> Option.map (fun scheme -> (name, scheme) :: acc)
        |> Option.value ~default:acc)
      [] module_.top_declarations
  in
  Interface.of_module ~values module_
