open Typed
open Typed.Type
module Name_map = Map.Make (Data.Name)
module Map = Map.Make (String)

type ctx = scheme Name_map.t

type type_env = {
  types : Canonical.Typedecl.t Name_map.t;
  constructors :
    (Canonical.Typedecl.t * Canonical.Typedecl.type_ctor) Name_map.t;
}

type ctor_info = { name : Data.Name.t; arity : int; index : int; total : int }

type infer_result = {
  ctx : ctx;
  declarations : Typed.Declaration.t list;
  arity_env : int Name_map.t;
  siblings_env : (Data.Name.t * int) list Name_map.t;
  constructors : ctor_info list;
}

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
  {
    types = List.fold_left add_type Name_map.empty visible;
    constructors = List.fold_left add_constructors Name_map.empty visible;
  }

let primitive_ctx : ctx =
  List.fold_left
    (fun acc (name, scheme) -> Name_map.add (Data.Name.local name) scheme acc)
    Name_map.empty Primitives.values

open Canonical.Expr
module Str_set = Set.Make (String)

let rec ftv_typ = function
  | TVar v -> Str_set.singleton v
  | TInt | TBool | TStr | TUnit | TRowEmpty -> Str_set.empty
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

let rec apply_typ ty s =
  match ty with
  | TVar v -> ( match Map.find_opt v s with Some t -> t | None -> ty)
  | TInt | TBool | TStr | TUnit | TRowEmpty -> ty
  | TFun (p, r) -> TFun (apply_typ p s, apply_typ r s)
  | TTup l -> TTup (List.map (fun ty -> apply_typ ty s) l)
  | TCustom (name, typs) ->
      TCustom (name, List.map (fun ty -> apply_typ ty s) typs)
  | TRecord t -> TRecord (apply_typ t s)
  | TRowExtend (l, t, r) -> TRowExtend (l, apply_typ t s, apply_typ r s)

let string_of_typ ty =
  let rec str_simple ty =
    match ty with
    | TVar v -> v
    | TInt -> "Int"
    | TBool -> "Bool"
    | TUnit -> "Unit"
    | TStr -> "String"
    | TFun (p, r) -> str_paren p ^ " -> " ^ str_simple r
    | TTup l ->
        let buf = Buffer.create 50 in
        let append ty = str_paren ty |> Buffer.add_string buf in
        let rec iter = function
          | [] -> ()
          | [ ty ] -> append ty
          | h :: t ->
              append h;
              Buffer.add_string buf " * ";
              iter t
        in
        iter l;
        Buffer.contents buf
    | TCustom (name, args) ->
        let args_str =
          match args with
          | [] -> ""
          | _ ->
              let args_list = List.map str_paren args in
              let args_str = String.concat ", " args_list in
              "<" ^ args_str ^ ">"
        in
        Data.Name.to_string name ^ args_str
    | TRecord row ->
        let rec str_row first = function
          | TRowEmpty -> ""
          | TVar v -> if first then v ^ " | " else " | " ^ v
          | TRowExtend (label, typ, rest) -> (
              match rest with
              | TVar v ->
                  let prefix = if first then v ^ " | " else ", " in
                  prefix ^ Printf.sprintf "%s : %s" label (str_simple typ)
              | TRowEmpty ->
                  let sep = if first then "" else ", " in
                  sep ^ Printf.sprintf "%s : %s" label (str_simple typ)
              | _ ->
                  let sep = if first then "" else ", " in
                  sep
                  ^ Printf.sprintf "%s : %s" label (str_simple typ)
                  ^ str_row false rest)
          | _ -> failwith "Invalid row type in record"
        in
        Printf.sprintf "{ %s }" (str_row true row)
    | TRowExtend (label, typ, row) ->
        Printf.sprintf "%s : %s | %s" label (str_simple typ) (str_simple row)
    | TRowEmpty -> ""
  and str_paren ty =
    match ty with
    | TFun (_, _) | TTup _ -> "(" ^ str_simple ty ^ ")"
    | _ -> str_simple ty
  in
  str_simple ty

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

let bind_var ty v =
  match ty with
  | TVar v' when v = v' -> Map.empty
  | _ ->
      if Str_set.mem v (ftv_typ ty) then
        Printf.sprintf "Occurs check failed for %s in %s" v (string_of_typ ty)
        |> failwith
      else Map.singleton v ty

let rec rewrite_row row label =
  match row with
  | TRowEmpty -> failwith (Printf.sprintf "label %s cannot be inserted" label)
  | TRowExtend (label2, ty, tail) when label = label2 -> (ty, tail, Map.empty)
  | TRowExtend (label2, ty, tail) -> (
      match tail with
      | TVar a ->
          let new_r = new_var "r" in
          let new_a = new_var "a" in
          ( new_a,
            TRowExtend (label2, ty, new_r),
            Map.singleton a (TRowExtend (label, new_a, new_r)) )
      | _ ->
          let ty2, tail2, s = rewrite_row tail label in
          (ty2, TRowExtend (label2, ty, tail2), s))
  | _ -> failwith (Printf.sprintf "Unexpected type: %s" (Type.show row))

let rec unify ty1 ty2 =
  let unify_err ty1 ty2 =
    let ty1' = string_of_typ ty1 and ty2' = string_of_typ ty2 in
    Printf.sprintf "Unification failed for %s and %s" ty1' ty2' |> failwith
  in
  let rec unify' = function
    | TVar v, ty | ty, TVar v -> bind_var ty v
    | TInt, TInt
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
        else
          List.fold_left2
            (fun acc ty1 ty2 ->
              unify' (apply_typ ty1 acc, apply_typ ty2 acc) ++ acc)
            Map.empty l l'
    | TCustom (name1, args1), TCustom (name2, args2) ->
        if name1 = name2 then
          List.fold_left2
            (fun acc ty1 ty2 ->
              unify' (apply_typ ty1 acc, apply_typ ty2 acc) ++ acc)
            Map.empty args1 args2
        else unify_err ty1 ty2
    | TRecord ty1, TRecord ty2 -> unify' (ty1, ty2)
    | TRowEmpty, TRowExtend (label, _, _) ->
        failwith (Printf.sprintf "Extra field '%s' in record" label)
    | TRowExtend (label, _, _), TRowEmpty ->
        failwith (Printf.sprintf "Missing field '%s' in record" label)
    | TRowExtend (l1, ty1, rt1), (TRowExtend (_, _, _) as row2) -> (
        let ty2, rt2, s1 = rewrite_row row2 l1 in
        let rec to_list ty =
          match ty with
          | TVar name -> ([], Some name)
          | TRowEmpty -> ([], None)
          | TRowExtend (l, t, r) ->
              let ls, mv = to_list r in
              ((l, t) :: ls, mv)
          | _ -> failwith (Printf.sprintf "invalid row tail %s" (Type.show ty))
        in
        let result = to_list rt1 in
        match snd result with
        | Some tv when Map.mem tv s1 -> failwith "recursive row type"
        | _ ->
            let s2 = unify' (apply_typ ty1 s1, apply_typ ty2 s1) in
            let s3 = s2 ++ s1 in
            let s4 = unify' (apply_typ rt1 s3, apply_typ rt2 s3) in
            s4 ++ s3)
    | _ -> unify_err ty1 ty2
  in
  unify' (ty1, ty2)

let instantiate = function
  | Scheme (vars, ty) ->
      let nvars = List.map (fun _ -> new_var "a") vars in
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
    | Kind.Tkind_function fn -> (
        match List.rev fn.arguments with
        | [] -> failwith "Empty function type"
        | return_impl :: rev_params ->
            List.fold_left
              (fun acc (param : Impl.t) -> TFun (conv param, acc))
              (conv return_impl) rev_params)
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
            List.fold_right
              (fun arg_ty acc -> TFun (arg_ty, acc))
              param_types result_type
      in
      Name_map.add ctor_name (Scheme (typedef.params, ctor_type)) acc)
    type_env.constructors primitive_ctx

let rec infer_with_env (exp : Canonical.Expr.t) ctx (type_env : type_env) :
    Type.t Map.t * Typed.Expr.t =
  let infer exp ctx = infer_with_env exp ctx type_env in
  match exp with
  | Expr_int i -> (Map.empty, { expr = Typed.Expr.Expr_int i; typ = TInt })
  | Expr_float f -> (Map.empty, { expr = Expr_float f; typ = TInt })
  | Expr_string s -> (Map.empty, { expr = Expr_string s; typ = TStr })
  | Expr_char c -> (Map.empty, { expr = Expr_char c; typ = TStr })
  | Expr_unit -> (Map.empty, { expr = Typed.Expr.Expr_unit; typ = TUnit })
  | Expr_kernel primitive ->
      ( Map.empty,
        { expr = Typed.Expr.Expr_kernel primitive; typ = new_var "a" } )
  | Expr_ident v -> (
      match Name_map.find_opt v ctx with
      | Some s ->
          let _, ty = instantiate s in
          (Map.empty, { expr = Expr_ident v; typ = ty })
      | None ->
          Printf.sprintf "Unbound value %s" (Data.Name.to_string v) |> failwith)
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
      let s, typed_elems, ty =
        List.fold_left
          (fun (s_acc, elems_acc, ty_acc) expr ->
            let s, typed_expr = infer expr (apply_ctx ctx s_acc) in
            let s_unify = unify (apply_typ elem_type s_acc) typed_expr.typ in
            let final_elem_typ = apply_typ elem_type s_unify in
            (s_unify ++ s ++ s_acc, typed_expr :: elems_acc, final_elem_typ))
          (Map.empty, [], elem_type) l
      in
      ( s,
        {
          expr = Expr_list (List.rev typed_elems);
          typ = TCustom (Data.Name.local "List", [ ty ]);
        } )
  | Expr_let { binding = { bind_body = { name; body = rhs } }; body } ->
      let self_ty = new_var "a" in
      let ctx_rec =
        Name_map.add (Data.Name.local name.thing) (Scheme ([], self_ty)) ctx
      in
      let s_rhs, t1 = infer rhs ctx_rec in
      let s_self = unify (apply_typ self_ty s_rhs) t1.typ in
      let s1 = s_self ++ s_rhs in
      let t1 = { t1 with Typed.Expr.typ = apply_typ t1.typ s_self } in
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
      let s0, t0 = infer expr ctx in

      let m_pattern (pattern : Canonical.Pattern.t) =
        let rec go = function
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
              (Map.empty, { typ = TInt; pattern = P_T_int i }, Name_map.empty)
          | P_str s ->
              (Map.empty, { typ = TStr; pattern = P_T_str s }, Name_map.empty)
          | P_chr c ->
              (Map.empty, { typ = TStr; pattern = P_T_chr c }, Name_map.empty)
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
                    let s_unify = unify (apply_typ elem_type s) ty.typ in
                    ( s_unify ++ s ++ s_acc,
                      ty :: res_acc,
                      merge_ctx ctx_acc ctx' ))
                  (Map.empty, [], Name_map.empty) list
              in
              ( s,
                {
                  typ = TCustom (Data.Name.local "List", [ elem_type ]);
                  pattern = P_T_list (List.rev resolved);
                },
                ctx )
          | P_cons (head, tail) ->
              let s1, ty_head, ctx1 = go head in
              let s2, ty_tail, ctx2 = go tail in
              let list_type = TCustom (Data.Name.local "List", [ ty_head.typ ]) in
              let s3 = unify (apply_typ ty_tail.typ s2) list_type in
              ( s3 ++ s2 ++ s1,
                { typ = list_type; pattern = P_T_cons (ty_head, ty_tail) },
                merge_ctx ctx1 ctx2 )
          | P_ctor (name, list) -> (
              let decl = Name_map.find_opt name type_env.constructors in
              match decl with
              | Some (type_, d) ->
                  let s, resolved_types, patterns, ctx =
                    List.fold_left2
                      (fun (acc, m, patterns, ctx)
                           (pattern : Canonical.Pattern.t)
                           (ctor : Canonical.Typedef.Impl.t) ->
                        let s, ty_arg, ctx' = go pattern in
                        ( acc ++ s,
                          (match ctor.body with
                          | Canonical.Typedef.Kind.Tkind_var v ->
                              Map.add v.thing ty_arg.typ m
                          | _ -> m),
                          ty_arg :: patterns,
                          merge_ctx ctx ctx' ))
                      (Map.empty, Map.empty, [], Name_map.empty)
                      list d.data
                  in
                  let args =
                    List.map
                      (fun p ->
                        resolved_types |> Map.find_opt p
                        |> CCOption.get_lazy (fun () -> new_var "a"))
                      type_.params
                  in
                  ( s,
                    {
                      typ = concrete_type type_.name args;
                      pattern = P_T_ctor (name, List.rev patterns);
                    },
                    ctx )
              | None ->
                  failwith
                    (Printf.sprintf "Unknown constructor %s"
                       (Data.Name.to_string name)))
          | P_record fields ->
              let row_var = new_var "r" in
              let row_type =
                List.fold_right
                  (fun field_name acc ->
                    TRowExtend (field_name, new_var "a", acc))
                  fields row_var
              in
              ( Map.empty,
                { typ = TRecord row_type; pattern = P_T_record fields },
                Name_map.empty )
        in
        go pattern
      in

      let fn (patterns, (s0, ty0), (s1, ty1), typed_cases)
          { pattern; expr = case_expr } =
        let fn_apply (s2, ty2, ctx') =
          let ctx = Name_map.union (fun _ _ b -> Some b) ctx ctx' in
          let s3, t3 = infer case_expr (apply_ctx ctx s2) in
          let s4 =
            unify (apply_typ ty2.Pattern.typ (s3 ++ s2)) (apply_typ ty0 s0)
          in
          let s5 = unify (apply_typ t3.typ s3) (apply_typ ty1 s1) in
          let s4' = s4 ++ s3 ++ s2 ++ s1 ++ s0 in
          let typed_case = { Typed.Expr.pattern = ty2; expr = t3 } in
          ( ty2 :: patterns,
            (s4', apply_typ ty2.typ s2),
            (s5 ++ s4', apply_typ t3.typ s5),
            typed_case :: typed_cases )
        in
        fn_apply @@ m_pattern pattern
      in

      let patterns, (s1, t1), (s2, ty2), typed_cases =
        match pattern_data_items with
        | hd :: rest ->
            List.fold_left fn
              (fn ([], (s0, t0.typ), (Map.empty, new_var "a"), []) hd)
              rest
        | [] -> raise (failwith "no patterns")
      in

      ( s2 ++ s1,
        {
          expr =
            Expr_pattern
              { expr = t0; pattern_data_items = List.rev typed_cases };
          typ = ty2;
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
      let fn_ty =
        List.fold_right
          (fun param_ty acc -> TFun (param_ty, acc))
          param_types' typed_body.typ
      in
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
  | _ -> failwith "Expression not implemented"

let infer_exp exp ctx type_env =
  State.reset ();
  let s, ty = infer_with_env exp ctx type_env in
  (* FIXME *)
  apply_typ ty.typ s

let infer' exp ctx type_env = infer_with_env exp ctx type_env

let infer exp =
  infer_exp exp primitive_ctx
    { types = Name_map.empty; constructors = Name_map.empty }

let infer_declaration { Canonical.Declaration.body_part; type_part_data } ctx
    type_env =
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
    infer' body_part.expr.Data.Located.thing ctx_with_params type_env
  in

  let param_types' = List.map (fun ty -> apply_typ ty s) param_types in

  let final_ty =
    List.fold_right
      (fun param_ty acc -> TFun (param_ty, acc))
      param_types'
      (apply_typ typed_expr.typ s)
  in

  let verified_ty, s_final =
    match type_part_data with
    | None -> (final_ty, s)
    | Some type_part ->
        let declared_ty = typedef_to_type type_part.type_alias in
        let s_check = unify final_ty declared_ty in
        (apply_typ final_ty s_check, s_check ++ s)
  in

  let scheme = generalize verified_ty ctx in
  let new_ctx =
    Name_map.add (Data.Name.local body_part.name.thing) scheme ctx
  in

  let typed_params =
    List.map2
      (fun name typ -> { Typed.Declaration.name; typ })
      body_part.params param_types'
  in

  let typed_decl =
    {
      Typed.Declaration.name = body_part.name;
      params = typed_params;
      body = typed_expr;
      typ = verified_ty;
    }
  in

  (s_final, typed_decl, new_ctx)

let build_arity_env (type_env : type_env) : int Name_map.t =
  Name_map.fold
    (fun _ctor_name (typedef, _ctor) acc ->
      let arity = List.length typedef.Canonical.Typedecl.ctors in
      List.fold_left
        (fun acc_inner (ctor : Canonical.Typedecl.type_ctor) ->
          Name_map.add ctor.id arity acc_inner)
        acc typedef.ctors)
    type_env.constructors Name_map.empty

let infer_toplevel ~(imports : Interface.t list)
    (module_ : Canonical.Module.t) initial_ctx =
  let type_env = build_type_env ~imports module_ in

  let ctx_with_constructors = build_initial_ctx type_env in

  let initial_ctx =
    Name_map.union
      (fun _ _ user_val -> Some user_val)
      ctx_with_constructors initial_ctx
  in

  let initial_ctx =
    List.concat_map (fun (i : Interface.t) -> i.values) imports
    |> List.fold_left
         (fun acc (value : Interface.value) ->
           Name_map.add value.name value.scheme acc)
         initial_ctx
  in

  let imported_aliases =
    List.concat_map (fun (i : Interface.t) -> i.type_aliases) imports
    |> List.fold_left
         (fun acc (ta : Canonical.Typealias.t) ->
           Name_map.add ta.name (ta.params, typedef_to_type ta.typedef) acc)
         Name_map.empty
  in

  let type_aliases =
    Canonical.Module.String_map.fold
      (fun name ta acc ->
        let alias_type = typedef_to_type ta.Canonical.Typealias.typedef in
        Name_map.add (Data.Name.local name) (ta.params, alias_type) acc)
      module_.type_aliases imported_aliases
  in

  let rec expand_type_alias ty =
    match ty with
    | TCustom (name, args) -> (
        match Name_map.find_opt name type_aliases with
        | Some (params, alias_body) ->
            if List.length params = List.length args then
              let subst =
                List.fold_left2
                  (fun acc param arg ->
                    Map.add param (expand_type_alias arg) acc)
                  Map.empty params args
              in
              expand_type_alias (apply_typ alias_body subst)
            else if List.length params = 0 && List.length args = 0 then
              expand_type_alias alias_body
            else
              failwith
                (Printf.sprintf "Type alias %s expects %d arguments, got %d"
                   (Data.Name.to_string name)
                   (List.length params) (List.length args))
        | None -> TCustom (name, List.map expand_type_alias args))
    | TFun (p, r) -> TFun (expand_type_alias p, expand_type_alias r)
    | TTup l -> TTup (List.map expand_type_alias l)
    | TRecord t -> TRecord (expand_type_alias t)
    | TRowExtend (label, t, r) ->
        TRowExtend (label, expand_type_alias t, expand_type_alias r)
    | t -> t
  in

  let infer_declaration_with_expansion
      { Canonical.Declaration.body_part; type_part_data } ctx =
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
      infer' body_part.expr.Data.Located.thing ctx_with_params type_env
    in

    let param_types' = List.map (fun ty -> apply_typ ty s) param_types in

    let final_ty =
      List.fold_right
        (fun param_ty acc -> TFun (param_ty, acc))
        param_types'
        (apply_typ typed_expr.typ s)
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

    let scheme = generalize verified_ty ctx in
    let new_ctx =
    Name_map.add (Data.Name.local body_part.name.thing) scheme ctx
  in

    let typed_params =
      List.map2
        (fun name typ -> { Typed.Declaration.name; typ })
        body_part.params param_types'
    in

    let typed_decl =
      {
        Typed.Declaration.name = body_part.name;
        params = typed_params;
        body = typed_expr;
        typ = verified_ty;
      }
    in

    (s_final, typed_decl, new_ctx)
  in

  let ctx_with_placeholders =
    Canonical.Module.String_map.fold
      (fun name decl acc ->
        let ty_scheme =
          match decl.Canonical.Declaration.type_part_data with
          | Some type_part ->
              let declared_ty = typedef_to_type type_part.type_alias in
              let expanded_ty = expand_type_alias declared_ty in
              generalize expanded_ty Name_map.empty
          | None -> Scheme ([], new_var "a")
        in
        Name_map.add (Data.Name.local name) ty_scheme acc)
      module_.top_declarations initial_ctx
  in

  let final_ctx, typed_decls =
    Canonical.Module.String_map.fold
      (fun name decl_data (ctx_acc, decls_acc) ->
        let s, typed_decl, new_ctx =
          infer_declaration_with_expansion decl_data ctx_acc
        in
        (new_ctx, typed_decl :: decls_acc))
      module_.top_declarations
      (ctx_with_placeholders, [])
  in

  let arity_env = build_arity_env type_env in

  let siblings_env =
    Name_map.to_seq type_env.types
    |> Seq.concat_map (fun (_, (td : Canonical.Typedecl.t)) ->
           let sibs =
             List.map
               (fun (c : Canonical.Typedecl.type_ctor) -> (c.id, List.length c.data))
               td.ctors
           in
           List.to_seq td.ctors
           |> Seq.map (fun (c : Canonical.Typedecl.type_ctor) -> (c.id, sibs)))
    |> Name_map.of_seq
  in

  let constructors =
    Name_map.fold
      (fun _ (td : Canonical.Typedecl.t) acc ->
        let total = List.length td.ctors in
        List.mapi
          (fun index (c : Canonical.Typedecl.type_ctor) ->
            { name = c.id; arity = List.length c.data; index; total })
          td.ctors
        @ acc)
      type_env.types []
  in

  {
    ctx = final_ctx;
    declarations = List.rev typed_decls;
    arity_env;
    siblings_env;
    constructors;
  }

let interface_of (module_ : Canonical.Module.t) (result : infer_result) :
    Interface.t =
  let values =
    Canonical.Module.String_map.fold
      (fun name _ acc ->
        Name_map.find_opt (Data.Name.local name) result.ctx
        |> Option.map (fun scheme -> (name, scheme) :: acc)
        |> Option.value ~default:acc)
      module_.top_declarations []
  in
  Interface.of_module ~values module_
