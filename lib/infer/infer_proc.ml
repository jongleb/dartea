open Typed
open Typed.Type
module Name_map = Data.Name.Map
module Map = Typed.Type.By_variable

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

let apply_typ ty s = substitute s ty

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
                  { param with typ = apply_typ param.typ s })
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
  { typ = apply_typ e.typ s; expr }

let substitute_declaration (decl : Typed.Declaration.t) s =
  let parameter (param : Typed.Declaration.param) =
    { param with typ = apply_typ param.typ s }
  in
  {
    decl with
    params = List.map parameter decl.params;
    body = substitute_expr decl.body s;
    typ = apply_typ decl.typ s;
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

let apply_scheme scheme s =
  match scheme with
  | Scheme (vars, ty) ->
      let s' = List.fold_right (fun v acc -> Map.remove v acc) vars s in
      Scheme (vars, apply_typ ty s')

let apply_ctx ctx s = Name_map.map (fun scheme -> apply_scheme scheme s) ctx

let ftv_scheme = function
  | Scheme (vars, ty) -> Str_set.diff (ftv_typ ty) (Str_set.of_list vars)

let ftv_ctx ctx =
  Name_map.fold
    (fun _ scheme acc -> Str_set.union acc (ftv_scheme scheme))
    ctx Str_set.empty

let generalize ty ctx =
  let vars = Str_set.diff (ftv_typ ty) (ftv_ctx ctx) |> Str_set.to_seq in
  Scheme (List.of_seq vars, ty)

let compose s1 s2 =
  Map.map (fun t -> apply_typ t s1) s2 |> Map.union (fun _ x _ -> Some x) s1

let ( ++ ) = compose

module State = struct
  let state = ref 0

  let next () =
    let id = !state in
    incr state;
    id

  let reset () = state := 0
end

let new_var pref = TVar (State.next () |> Printf.sprintf "%s%i" pref)
let numeric_literal () = new_var (Data.Constraint.name Data.Constraint.Number)

let list_element_of name arguments =
  match arguments with
  | [ element ] when Data.Name.equal name (Data.Name.local "List") ->
      Some element
  | _ -> None

let combining left right =
  match Data.Constraint.combined left right with
  | Some together -> together
  | None ->
      Printf.sprintf "%s and %s cannot be the same type variable"
        (Data.Constraint.name left) (Data.Constraint.name right)
      |> failwith

let narrowed required variable =
  let renamed together =
    Map.singleton variable (new_var (Data.Constraint.name together))
  in
  match Data.Constraint.of_variable variable with
  | None -> renamed required
  | Some carried ->
      let together = combining carried required in
      if together = carried then Map.empty else renamed together

let rec satisfying (required : Data.Constraint.t) ty =
  let unsatisfied () =
    Printf.sprintf "%s does not satisfy %s" (string_of_typ ty)
      (Data.Constraint.name required)
    |> failwith
  in
  let every constrained items =
    List.fold_left
      (fun acc item -> satisfying constrained (apply_typ item acc) ++ acc)
      Map.empty items
  in
  match (required, ty) with
  | _, TVar variable -> narrowed required variable
  | Number, (TInt | TFloat) -> Map.empty
  | Comparable, (TInt | TFloat | TChar | TStr) -> Map.empty
  | (Appendable | Comp_appendable), TStr -> Map.empty
  | Comparable, TTup items -> every Comparable items
  | ( (Number | Appendable | Comparable | Comp_appendable),
      TCustom (name, arguments) ) -> begin
      match (required, list_element_of name arguments) with
      | _, None -> unsatisfied ()
      | Appendable, Some _ -> Map.empty
      | (Comparable | Comp_appendable), Some element ->
          satisfying Comparable element
      | Number, Some _ -> unsatisfied ()
    end
  | ( (Number | Comparable | Appendable | Comp_appendable),
      ( TInt | TFloat | TChar | TStr | TBool | TUnit | TFun _ | TTup _
      | TRecord _ | TRowExtend _ | TRowEmpty ) ) ->
      unsatisfied ()

let unify_variables left right =
  if String.equal left right then Map.empty
  else
    match
      (Data.Constraint.of_variable left, Data.Constraint.of_variable right)
    with
    | None, _ -> Map.singleton left (TVar right)
    | Some _, None -> Map.singleton right (TVar left)
    | Some carried_left, Some carried_right ->
        let together = combining carried_left carried_right in
        if together = carried_left then Map.singleton right (TVar left)
        else if together = carried_right then Map.singleton left (TVar right)
        else
          let fresh = new_var (Data.Constraint.name together) in
          Map.add right fresh (Map.singleton left fresh)

let bind_var ty v =
  match ty with
  | TVar v' when String.equal v v' -> Map.empty
  | _ ->
      if Str_set.mem v (ftv_typ ty) then
        Printf.sprintf "Occurs check failed for %s in %s" v (string_of_typ ty)
        |> failwith
      else begin
        match Data.Constraint.of_variable v with
        | None -> Map.singleton v ty
        | Some required ->
            let narrowing = satisfying required ty in
            Map.singleton v (apply_typ ty narrowing) ++ narrowing
      end

let rec rewrite_row row label =
  match row with
  | TRowEmpty -> failwith (Printf.sprintf "label %s cannot be inserted" label)
  | TRowExtend (label2, ty, tail) when label = label2 -> (ty, tail, Map.empty)
  | TRowExtend (label2, ty, tail) -> begin
      match tail with
      | TVar a ->
          let new_r = new_var "r" in
          let new_a = new_var "a" in
          ( new_a,
            TRowExtend (label2, ty, new_r),
            Map.singleton a (TRowExtend (label, new_a, new_r)) )
      | _ ->
          let ty2, tail2, s = rewrite_row tail label in
          (ty2, TRowExtend (label2, ty, tail2), s)
    end
  | TVar _ | TInt | TFloat | TChar | TBool | TStr | TUnit | TFun _ | TTup _
  | TCustom _ | TRecord _ ->
      failwith
        (Printf.sprintf "%s is not a record and has no field %s"
           (string_of_typ row) label)

let rec unify ty1 ty2 =
  let unify_err ty1 ty2 =
    let ty1' = string_of_typ ty1 and ty2' = string_of_typ ty2 in
    Printf.sprintf "Unification failed for %s and %s" ty1' ty2' |> failwith
  in
  let rec unify_in_order left right =
    List.fold_left2
      (fun acc ty1 ty2 -> unify' (apply_typ ty1 acc, apply_typ ty2 acc) ++ acc)
      Map.empty left right

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
        Map.empty
    | TFun (p, r), TFun (p', r') ->
        let s1 = unify' (p, p') in
        let s2 = unify' (apply_typ r s1, apply_typ r' s1) in
        s2 ++ s1
    | TTup l, TTup l' ->
        if List.length l != List.length l' then unify_err ty1 ty2
        else unify_in_order l l'
    | TCustom (name1, args1), TCustom (name2, args2) ->
        if Data.Name.equal name1 name2 then unify_in_order args1 args2
        else unify_err ty1 ty2
    | TRecord ty1, TRecord ty2 -> unify' (ty1, ty2)
    | TRowEmpty, TRowExtend (label, _, _) ->
        failwith (Printf.sprintf "Extra field '%s' in record" label)
    | TRowExtend (label, _, _), TRowEmpty ->
        failwith (Printf.sprintf "Missing field '%s' in record" label)
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
              failwith
                (Printf.sprintf "%s cannot end a record" (string_of_typ ty))
        in
        let result = to_list rt1 in
        match snd result with
        | Some tv when Map.mem tv s1 -> failwith "recursive row type"
        | _ ->
            let s2 = unify' (apply_typ ty1 s1, apply_typ ty2 s1) in
            let s3 = s2 ++ s1 in
            let s4 = unify' (apply_typ rt1 s3, apply_typ rt2 s3) in
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
      let nvars = List.map (fun v -> new_var (variable_prefix v)) vars in
      let s =
        List.fold_left2 (fun acc v nv -> Map.add v nv acc) Map.empty vars nvars
      in
      (s, apply_typ ty s)

let merge_ctx ctx1 ctx2 =
  Name_map.union
    (fun key scheme1 scheme2 ->
      let s1, ty1 = instantiate scheme1 in
      let s2, ty2 = instantiate scheme2 in
      let s = unify (apply_typ ty1 s1) (apply_typ ty2 s2) in
      let final_ty = apply_typ ty1 (s ++ s1) in
      Some (Scheme ([], final_ty)))
    ctx1 ctx2

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

let build_initial_ctx (type_env : type_env) : ctx =
  Name_map.fold
    (fun ctor_name
         ( (typedef : Canonical.Typedecl.t),
           (ctor : Canonical.Typedecl.type_ctor) ) acc ->
      let result_type =
        concrete_type typedef.name (List.map (fun p -> TVar p) typedef.params)
      in
      let ctor_type =
        match ctor.data with
        | [] -> result_type
        | args ->
            let param_types =
              List.map
                (fun (arg_spec : Canonical.Typedef.Impl.t) ->
                  typedef_to_type arg_spec)
                args
            in
            function_of param_types ~result:result_type
      in
      Name_map.add ctor_name (Scheme (typedef.params, ctor_type)) acc)
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
              (fun acc param arg -> Map.add param (expand arg) acc)
              Map.empty params args
          in
          expand (apply_typ alias_body substitution)
      | Some (params, _) ->
          failwith
            (Printf.sprintf "Type alias %s expects %d arguments, got %d"
               (Data.Name.to_string name) (List.length params)
               (List.length args))
    end
  | TFun (p, r) -> TFun (expand p, expand r)
  | TTup l -> TTup (List.map expand l)
  | TRecord t -> TRecord (expand t)
  | TRowExtend (label, t, r) -> TRowExtend (label, expand t, expand r)
  | t -> t

let rec infer_pattern (type_env : type_env) (pattern : Canonical.Pattern.t) :
    Type.t Map.t * Typed.Pattern.t * ctx =
  let go = infer_pattern type_env in
  match pattern with
      | Canonical.Pattern.P_var v ->
          ( Map.empty,
            { Pattern.typ = new_var "a"; pattern = P_T_var v },
            Name_map.singleton (Data.Name.local v)
              (Scheme ([], new_var "a")) )
      | P_anything ->
          ( Map.empty,
            { typ = new_var "a"; pattern = P_T_anything },
            Name_map.empty )
      | P_int i ->
          ( Map.empty,
            { typ = numeric_literal (); pattern = P_T_int i },
            Name_map.empty )
      | P_str s ->
          (Map.empty, { typ = TStr; pattern = P_T_str s }, Name_map.empty)
      | P_chr c ->
          (Map.empty, { typ = TChar; pattern = P_T_chr c }, Name_map.empty)
      | P_unit ->
          (Map.empty, { typ = TUnit; pattern = P_T_unit }, Name_map.empty)
      | P_tuple list ->
          let s, resolved, ctx =
            List.fold_left
              (fun (s_acc, res_acc, ctx_acc) pat ->
                let s, ty, ctx' = go pat in
                (s ++ s_acc, ty :: res_acc, merge_ctx ctx_acc ctx'))
              (Map.empty, [], Name_map.empty) list
          in
          ( s,
            {
              typ = TTup (List.rev_map (fun t -> t.Pattern.typ) resolved);
              pattern = P_T_tuple (List.rev resolved);
            },
            ctx )
      | P_list list ->
          let elem_type = new_var "a" in
          let s, resolved, ctx =
            List.fold_left
              (fun (s_acc, res_acc, ctx_acc) pat ->
                let s, ty, ctx' = go pat in
                let s_acc = s ++ s_acc in
                let s_unify =
                  unify (apply_typ elem_type s_acc) (apply_typ ty.typ s_acc)
                in
                (s_unify ++ s_acc, ty :: res_acc, merge_ctx ctx_acc ctx'))
              (Map.empty, [], Name_map.empty) list
          in
          ( s,
            {
              typ = TCustom (Data.Name.local "List", [ apply_typ elem_type s ]);
              pattern =
                P_T_list
                  (List.rev_map
                     (Typed.Pattern.substitute s)
                     resolved);
            },
            ctx )
      | P_alias (inner, name) ->
          let s, aliased, bound = go inner in
          let typ = aliased.Pattern.typ in
          ( s,
            { typ; pattern = P_T_alias (aliased, name) },
            Name_map.add (Data.Name.local name) (Scheme ([], typ)) bound )
      | P_cons (head, tail) ->
          let s1, ty_head, ctx1 = go head in
          let s2, ty_tail, ctx2 = go tail in
          let list_type = TCustom (Data.Name.local "List", [ ty_head.typ ]) in
          let s3 = unify (apply_typ ty_tail.typ s2) list_type in
          ( s3 ++ s2 ++ s1,
            { typ = list_type; pattern = P_T_cons (ty_head, ty_tail) },
            merge_ctx ctx1 ctx2 )
      | P_ctor (name, arguments) -> begin
          match Name_map.find_opt name type_env.constructors with
          | None ->
              failwith
                (Printf.sprintf "Unknown constructor %s"
                   (Data.Name.to_string name))
          | Some (declared, ctor) ->
              let payload_variable (payload : Canonical.Typedef.Impl.t) =
                match payload.body with
                | Canonical.Typedef.Kind.Tkind_var written ->
                    Some (Data.Located.unwrap written)
                | _ -> None
              in
              let take (s_acc, bindings, patterns, bound) argument payload =
                let s, typed_argument, bound_here = go argument in
                let bindings =
                  match payload_variable payload with
                  | None -> bindings
                  | Some variable -> Map.add variable typed_argument.typ bindings
                in
                ( s_acc ++ s,
                  bindings,
                  typed_argument :: patterns,
                  merge_ctx bound bound_here )
              in
              let s, bindings, patterns, bound =
                List.fold_left2 take
                  (Map.empty, Map.empty, [], Name_map.empty)
                  arguments ctor.data
              in
              let argument_of parameter =
                match Map.find_opt parameter bindings with
                | Some typ -> typ
                | None -> new_var "a"
              in
              ( s,
                {
                  typ =
                    concrete_type declared.name
                      (List.map argument_of declared.params);
                  pattern = P_T_ctor (name, List.rev patterns);
                },
                bound )
        end
      | P_record fields ->
          let row_var = new_var "r" in
          let row_type, bound =
            List.fold_right
              (fun field_name (row, bound) ->
                let field_type = new_var "a" in
                ( TRowExtend (field_name, field_type, row),
                  Name_map.add
                    (Data.Name.local field_name)
                    (Scheme ([], field_type))
                    bound ))
              fields
              (row_var, Name_map.empty)
          in
          ( Map.empty,
            { typ = TRecord row_type; pattern = P_T_record fields },
            bound )

let rec infer_with_env (exp : Canonical.Expr.t) ctx (type_env : type_env) :
    Type.t Map.t * Typed.Expr.t =
  let infer exp ctx = infer_with_env exp ctx type_env in
  match exp with
  | Expr_int i ->
      (Map.empty, { expr = Typed.Expr.Expr_int i; typ = numeric_literal () })
  | Expr_float f -> (Map.empty, { expr = Expr_float f; typ = TFloat })
  | Expr_string s -> (Map.empty, { expr = Expr_string s; typ = TStr })
  | Expr_char c -> (Map.empty, { expr = Expr_char c; typ = TChar })
  | Expr_unit -> (Map.empty, { expr = Typed.Expr.Expr_unit; typ = TUnit })
  | Expr_kernel primitive ->
      ( Map.empty,
        { expr = Typed.Expr.Expr_kernel primitive; typ = new_var "a" } )
  | Expr_ident v -> begin
      match Name_map.find_opt v ctx with
      | Some scheme ->
          let _, typ = instantiate scheme in
          (Map.empty, { expr = Expr_ident v; typ })
      | None ->
          failwith
            (Printf.sprintf "Unbound value %s" (Data.Name.to_string v))
    end
  | Expr_apply { fn; arg } ->
      let s1, t1 = infer fn ctx in
      let s2, t2 = infer arg (apply_ctx ctx s1) in
      let ty_res = new_var "a" in
      let s3 = unify (apply_typ t1.typ s2) (TFun (t2.typ, ty_res)) in
      let final_typ = apply_typ ty_res s3 in
      ( s3 ++ s2 ++ s1,
        { expr = Expr_apply { fn = t1; arg = t2 }; typ = final_typ } )
  | Expr_if_then_else { if_exp; then_exp; else_exp } ->
      let s1, t1 = infer if_exp ctx in
      let s2 = unify (apply_typ t1.typ s1) TBool in
      let s3, t3 = infer then_exp (apply_ctx ctx (s2 ++ s1)) in
      let s4, t4 = infer else_exp (apply_ctx ctx (s3 ++ s2 ++ s1)) in
      let s5 = unify (apply_typ t3.typ s4) t4.typ in
      let final_typ = apply_typ t4.typ s5 in
      ( s5 ++ s4 ++ s3 ++ s2 ++ s1,
        {
          expr = Expr_if_then_else { if_exp = t1; then_exp = t3; else_exp = t4 };
          typ = final_typ;
        } )
  | Expr_list l ->
      let elem_type = new_var "a" in
      let s, typed_elems =
        List.fold_left
          (fun (s_acc, elems_acc) expr ->
            let s, typed_expr = infer expr (apply_ctx ctx s_acc) in
            let s_acc = s ++ s_acc in
            let s_unify =
              unify (apply_typ elem_type s_acc) (apply_typ typed_expr.typ s_acc)
            in
            (s_unify ++ s_acc, typed_expr :: elems_acc))
          (Map.empty, []) l
      in
      ( s,
        {
          expr =
            Expr_list
              (List.rev_map (fun item -> substitute_expr item s) typed_elems);
          typ = TCustom (Data.Name.local "List", [ apply_typ elem_type s ]);
        } )
  | Expr_record_update { record; fields } ->
      let s1, typed_record = infer record ctx in
      let s, reversed =
        List.fold_left
          (fun (s_acc, acc) (row : Canonical.Expr.expr_record_row) ->
            let s_value, typed_value = infer row.value (apply_ctx ctx s_acc) in
            let s_acc = s_value ++ s_acc in
            let others = new_var "r" in
            let s_field =
              unify
                (apply_typ typed_record.typ s_acc)
                (TRecord (TRowExtend (row.name, typed_value.typ, others)))
            in
            ( s_field ++ s_acc,
              { Typed.Expr.name = row.name; value = typed_value } :: acc ))
          (s1, []) fields
      in
      let substituted (row : Typed.Expr.expr_record_row) =
        { row with Typed.Expr.value = substitute_expr row.value s }
      in
      ( s,
        {
          expr =
            Expr_record_update
              {
                record = substitute_expr typed_record s;
                fields = List.rev_map substituted reversed;
              };
          typ = apply_typ typed_record.typ s;
        } )
  | Expr_tuple items ->
      let s, reversed =
        List.fold_left
          (fun (s_acc, acc) item ->
            let s, typed_item = infer item (apply_ctx ctx s_acc) in
            (s ++ s_acc, typed_item :: acc))
          (Map.empty, []) items
      in
      let items = List.rev_map (fun item -> substitute_expr item s) reversed in
      let component (item : Typed.Expr.t) = item.typ in
      (s, { expr = Expr_tuple items; typ = TTup (List.map component items) })
  | Expr_cons { head; tail } ->
      let s_head, typed_head = infer head ctx in
      let s_tail, typed_tail = infer tail (apply_ctx ctx s_head) in
      let element = apply_typ typed_head.typ s_tail in
      let list_type = TCustom (Data.Name.local "List", [ element ]) in
      let s_list = unify typed_tail.typ list_type in
      let typ = apply_typ list_type s_list in
      ( s_list ++ s_tail ++ s_head,
        { expr = Expr_cons { head = typed_head; tail = typed_tail }; typ } )
  | Expr_let { binding = { bind_type; bind_body = { name; body = rhs } }; body }
    ->
      let self_ty = new_var "a" in
      let ctx_rec =
        Name_map.add (Data.Name.local name.thing) (Scheme ([], self_ty)) ctx
      in
      let s_rhs, t1 = infer rhs ctx_rec in
      let s_self = unify (apply_typ self_ty s_rhs) t1.typ in
      let s_declared =
        match bind_type with
        | None -> Map.empty
        | Some annotation ->
            unify
              (apply_typ t1.typ s_self)
              (expand_type_alias type_env (typedef_to_type annotation.content))
      in
      let s1 = s_declared ++ s_self ++ s_rhs in
      let t1 =
        { t1 with Typed.Expr.typ = apply_typ t1.typ (s_declared ++ s_self) }
      in
      let ctx' = apply_ctx ctx s1 in
      let gen_ty = generalize t1.typ ctx' in
      let ctx'' = Name_map.add (Data.Name.local name.thing) gen_ty ctx' in
      let s2, t2 = infer body ctx'' in
      ( s2 ++ s1,
        {
          expr =
            Expr_let
              { binding = { bind_body = { name; body = t1 } }; body = t2 };
          typ = t2.typ;
        } )
  | Expr_pattern { expr; pattern_data_items } ->
      let scrutinee_substitution, scrutinee = infer expr ctx in
      let branch
          ( (matched_substitution, matched_type),
            (result_substitution, result_type),
            cases ) { pattern; expr = branch_expr } =
        let pattern_substitution, typed_pattern, bound =
          infer_pattern type_env pattern
        in
        let ctx = Name_map.union (fun _ _ inner -> Some inner) ctx bound in
        let branch_substitution, typed_branch =
          infer branch_expr (apply_ctx ctx pattern_substitution)
        in
        let matches_the_scrutinee =
          unify
            (apply_typ typed_pattern.Pattern.typ
               (branch_substitution ++ pattern_substitution))
            (apply_typ matched_type matched_substitution)
        in
        let agrees_with_the_other_branches =
          unify
            (apply_typ typed_branch.typ branch_substitution)
            (apply_typ result_type result_substitution)
        in
        let matched_substitution =
          matches_the_scrutinee ++ branch_substitution ++ pattern_substitution
          ++ result_substitution ++ matched_substitution
        in
        ( ( matched_substitution,
            apply_typ typed_pattern.typ pattern_substitution ),
          ( agrees_with_the_other_branches ++ matched_substitution,
            apply_typ typed_branch.typ agrees_with_the_other_branches ),
          { Typed.Expr.pattern = typed_pattern; expr = typed_branch } :: cases )
      in
      let start =
        ((scrutinee_substitution, scrutinee.typ), (Map.empty, new_var "a"), [])
      in
      let (matched_substitution, _), (result_substitution, result_type), cases =
        match pattern_data_items with
        | first :: rest -> List.fold_left branch (branch start first) rest
        | [] -> failwith "A case expression needs at least one branch"
      in
      ( result_substitution ++ matched_substitution,
        {
          expr =
            Expr_pattern
              { expr = scrutinee; pattern_data_items = List.rev cases };
          typ = result_type;
        } )
  | Expr_record_extend label ->
      let a = new_var "a" in
      let r = new_var "r" in
      ( Map.empty,
        {
          expr = Expr_record_extend label;
          typ = TFun (a, TFun (TRecord r, TRecord (TRowExtend (label, a, r))));
        } )
  | Expr_record_empty ->
      (Map.empty, { expr = Expr_record_empty; typ = TRecord TRowEmpty })
  | Expr_record_select label ->
      let a = new_var "a" in
      let r = new_var "r" in
      ( Map.empty,
        {
          expr = Expr_record_select label;
          typ = TFun (TRecord (TRowExtend (label, a, r)), a);
        } )
  | Expr_accessor field ->
      let a = new_var "a" in
      let r = new_var "r" in
      let label = Data.Located.unwrap field in
      ( Map.empty,
        {
          expr = Expr_accessor field;
          typ = TFun (TRecord (TRowExtend (label, a, r)), a);
        } )
  | Expr_access { expr; field } ->
      let s1, t1 = infer expr ctx in
      let a = new_var "a" in
      let r = new_var "r" in
      let s2 =
        unify (apply_typ t1.typ s1) (TRecord (TRowExtend (field.thing, a, r)))
      in
      let final_typ = apply_typ a s2 in
      (s2 ++ s1, { expr = Expr_access { expr = t1; field }; typ = final_typ })
  | Expr_lambda { params; body } ->
      let param_types = List.map (fun _ -> new_var "a") params in
      let ctx_with_params =
        List.fold_left2
          (fun acc param param_ty ->
            Name_map.add
              (Data.Name.local param.Data.Located.thing)
              (Scheme ([], param_ty))
              acc)
          ctx params param_types
      in
      let s, typed_body = infer body ctx_with_params in
      let param_types' = List.map (fun ty -> apply_typ ty s) param_types in
      let fn_ty = function_of param_types' ~result:typed_body.typ in
      let typed_params =
        List.map2
          (fun p ty -> { Typed.Expr.name = p; typ = ty })
          params param_types'
      in
      ( s,
        {
          expr = Expr_lambda { params = typed_params; body = typed_body };
          typ = fn_ty;
        } )

let infer_toplevel ~(imports : Interface.t list)
    (module_ : Canonical.Module.t) primitives =
  let type_env = build_type_env ~imports module_ in
  let expand_type_alias = expand_type_alias type_env in

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
    let param_types = List.map (fun _ -> new_var "a") body_part.params in
    let ctx_with_params =
      List.fold_left2
        (fun acc param param_ty ->
          Name_map.add
          (Data.Name.local param.Data.Located.thing)
          (Scheme ([], param_ty))
          acc)
        ctx body_part.params param_types
    in

    let s, typed_expr =
      infer_with_env body_part.expr.Data.Located.thing ctx_with_params type_env
    in

    let settled_params = List.map (fun ty -> apply_typ ty s) param_types in

    let final_ty =
      function_of settled_params ~result:(apply_typ typed_expr.typ s)
    in

    let verified_ty, s_final =
      match type_part_data with
      | None -> (final_ty, s)
      | Some type_part ->
          let declared_ty = typedef_to_type type_part.type_alias in
          let expanded_declared_ty = expand_type_alias declared_ty in
          let s_check = unify final_ty expanded_declared_ty in
          (apply_typ final_ty s_check, s_check ++ s)
    in

    let typed_params =
      List.map2
        (fun name typ -> { Typed.Declaration.name; typ })
        body_part.params settled_params
    in

    let typed_decl =
      {
        Typed.Declaration.name = body_part.name;
        params = typed_params;
        body = typed_expr;
        typ = verified_ty;
      }
    in

    (s_final, typed_decl, verified_ty)
  in

  let infer_group ctx group =
    let member (position, (declaration : Canonical.Declaration.t)) =
      let name = Data.Name.local (Data.Located.unwrap declaration.body_part.name) in
      let assumed =
        match declaration.type_part_data with
        | Some _ -> None
        | None -> Some (new_var "a")
      in
      (position, declaration, name, assumed)
    in
    let members = List.map member group in
    let assuming ctx =
      List.fold_left
        (fun ctx (_, _, name, assumed) ->
          match assumed with
          | None -> ctx
          | Some typ -> Name_map.add name (Scheme ([], typ)) ctx)
        ctx members
    in
    let inside = assuming ctx in
    let infer_member (substitution, inferred) (position, declaration, name, assumed) =
      let inferred_substitution, typed, typ =
        infer_declaration declaration (apply_ctx inside substitution)
      in
      let substitution = inferred_substitution ++ substitution in
      let agreed =
        match assumed with
        | None -> substitution
        | Some assumption ->
            unify (apply_typ typ substitution) (apply_typ assumption substitution)
            ++ substitution
      in
      (agreed, (position, name, typed, (assumed, typ)) :: inferred)
    in
    let substitution, inferred =
      List.fold_left infer_member (Map.empty, []) members
    in
    let outside = apply_ctx ctx substitution in
    let settled = function
      | None, checked -> checked
      | Some assumption, _ -> apply_typ assumption substitution
    in
    let generalized =
      List.fold_left
        (fun ctx (_, name, _, typ) ->
          Name_map.add name (generalize (settled typ) outside) ctx)
        ctx inferred
    in
    ( generalized,
      List.rev_map
        (fun (position, _, typed, _) ->
          (position, substitute_declaration typed substitution))
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
                 (expand_type_alias (typedef_to_type annotation.type_alias))
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
