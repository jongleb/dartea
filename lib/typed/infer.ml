open Type
module Map = Map.Make (String)

type ctx = scheme Map.t

let typs : (string * scheme) list =
  [
    ("pow", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
    ("=", Scheme ([ "'a" ], TFun (TVar "'a", TFun (TVar "'a", TBool))));
    ("<>", Scheme ([ "'a" ], TFun (TVar "'a", TFun (TVar "'a", TBool))));
    ("&&", Scheme ([], TFun (TBool, TFun (TBool, TBool))));
    ("||", Scheme ([], TFun (TBool, TFun (TBool, TBool))));
    ("+", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
    ("plus", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
    ("concat", Scheme ([], TFun (TStr, TFun (TStr, TStr))));
    ("length", Scheme ([], TFun (TStr, TInt)));
    ("int_to_string", Scheme ([], TFun (TInt, TStr)));
    ("int_of_string", Scheme ([], TFun (TStr, TInt)));
    ("-", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
    ("*", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
    ("/", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
    ("id", Scheme ([ "'a" ], TFun (TVar "'a", TVar "'a")));
    ( "const",
      Scheme ([ "'a"; "'b" ], TFun (TVar "'a", TFun (TVar "'b", TVar "'a"))) );
    ( "pair",
      Scheme
        ( [ "'a"; "'b" ],
          TFun (TVar "'a", TFun (TVar "'b", TTup [ TVar "'a"; TVar "'b" ])) ) );
    ( "fst",
      Scheme ([ "'a"; "'b" ], TFun (TTup [ TVar "'a"; TVar "'b" ], TVar "'a"))
    );
    ( "snd",
      Scheme ([ "'a"; "'b" ], TFun (TTup [ TVar "'a"; TVar "'b" ], TVar "'b"))
    );
    ( "|>",
      Scheme
        ( [ "'a"; "'b" ],
          TFun (TVar "'a", TFun (TFun (TVar "'a", TVar "'b"), TVar "'b")) ) );
  ]

let ctx : ctx =
  let f acc (v, scheme) = Map.add v scheme acc in
  List.fold_left f Map.empty typs

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
        name ^ args_str
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

let apply_ctx ctx s = Map.map (fun scheme -> apply_scheme scheme s) ctx

let ftv_scheme = function
  | Scheme (vars, ty) -> Str_set.diff (ftv_typ ty) (Str_set.of_list vars)

let ftv_ctx ctx =
  Map.fold
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
  Map.union
    (fun key scheme1 scheme2 ->
      let s1, ty1 = instantiate scheme1 in
      let s2, ty2 = instantiate scheme2 in
      let s = unify (apply_typ ty1 s1) (apply_typ ty2 s2) in
      let final_ty = apply_typ ty1 (s ++ s1) in
      Some (Scheme ([], final_ty)))
    ctx1 ctx2

let typedef_to_type typedef =
  let open Canonical.Typedef in
  let rec convert = function
    | Kind.Tkind_var v -> TVar v.thing
    | Kind.Tkind_concrete c -> (
        match c.thing with
        | "Int" -> TInt
        | "Bool" -> TBool
        | "String" -> TStr
        | "Unit" -> TUnit
        | s -> TCustom (s, []))
    | Kind.Tkind_tuple types ->
        TTup (List.map (fun (impl : Impl.t) -> convert impl.body) types)
    | Kind.Tkind_function fn -> (
        match List.rev fn.arguments with
        | [] -> failwith "Empty function type"
        | return_impl :: rev_params ->
            List.fold_left
              (fun acc (param : Impl.t) -> TFun (convert param.body, acc))
              (convert return_impl.body) rev_params)
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
              TRowExtend (row.name.thing, convert row.body.body, acc))
            fields.values base
        in
        TRecord row_type
  in
  convert typedef

let rec infer (exp : Canonical.Expr.t) ctx =
  match exp with
  | Expr_int _ -> (Map.empty, TInt)
  | Expr_float _ -> (Map.empty, TInt)
  | Expr_string _ -> (Map.empty, TStr)
  | Expr_char _ -> (Map.empty, TStr)
  | Expr_ident v -> (
      match Map.find_opt v ctx with
      | Some s ->
          let _, ty = instantiate s in
          (Map.empty, ty)
      | None -> Printf.sprintf "Unbound value %s" v |> failwith)
  | Expr_apply { fn; arg } ->
      let s1, t1 = infer fn ctx in
      let s2, t2 = infer arg (apply_ctx ctx s1) in
      let ty_res = new_var "a" in
      let s3 = unify (apply_typ t1 s2) (TFun (t2, ty_res)) in
      (s3 ++ s2 ++ s1, apply_typ ty_res s3)
  | Expr_if_then_else { if_exp; then_exp; else_exp } ->
      let s1, t1 = infer if_exp ctx in
      let s2 = unify (apply_typ t1 s1) TBool in
      let s3, t3 = infer then_exp (apply_ctx ctx (s2 ++ s1)) in
      let s4, t4 = infer else_exp (apply_ctx ctx (s3 ++ s2 ++ s1)) in
      let s5 = unify (apply_typ t3 s4) t4 in
      (s5 ++ s4 ++ s3 ++ s2 ++ s1, apply_typ t4 s5)
  | Expr_list l ->
      let elem_type = new_var "a" in
      let s, ty =
        List.fold_left
          (fun (s_acc, ty_acc) expr ->
            let s, ty = infer expr (apply_ctx ctx s_acc) in
            let s_unify = unify (apply_typ elem_type s_acc) ty in
            (s_unify ++ s ++ s_acc, apply_typ elem_type s_unify))
          (Map.empty, elem_type) l
      in
      (s, TCustom ("List", [ ty ]))
  | Expr_let { binding = { bind_body = { name; body = rhs } }; body } ->
      let s1, t1 = infer rhs ctx in
      let ctx' = apply_ctx ctx s1 in
      let gen_ty = generalize t1 ctx' in
      let ctx'' = Map.add name.thing gen_ty ctx' in
      let s2, t2 = infer body ctx'' in
      (s2 ++ s1, t2)
  | Expr_pattern { expr; pattern_data_items } ->
      let s0, t0 = infer expr ctx in
      let m_pattern (pattern : Canonical.Pattern.t) =
        let rec go = function
          | Canonical.Pattern.P_var v ->
              ( Map.empty,
                { Pattern.Typed.typ = new_var "a"; pattern = P_T_var v },
                Map.singleton v (Scheme ([], new_var "a")) )
          | P_anything ->
              ( Map.empty,
                { typ = new_var "a"; pattern = P_T_anything },
                Map.empty )
          | P_int i ->
              (Map.empty, { typ = TInt; pattern = P_T_int i }, Map.empty)
          | P_str s ->
              (Map.empty, { typ = TStr; pattern = P_T_str s }, Map.empty)
          | P_chr c ->
              (Map.empty, { typ = TStr; pattern = P_T_chr c }, Map.empty)
          | P_unit -> (Map.empty, { typ = TUnit; pattern = P_T_unit }, Map.empty)
          | P_tuple list ->
              let s, resolved, ctx =
                List.fold_left
                  (fun (s_acc, res_acc, ctx_acc) pat ->
                    let s, ty, ctx' = go pat in
                    (s ++ s_acc, ty :: res_acc, merge_ctx ctx_acc ctx'))
                  (Map.empty, [], Map.empty) list
              in
              ( s,
                {
                  typ =
                    TTup (List.rev_map (fun t -> t.Pattern.Typed.typ) resolved);
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
                  (Map.empty, [], Map.empty) list
              in
              ( s,
                {
                  typ = TCustom ("List", [ elem_type ]);
                  pattern = P_T_list (List.rev resolved);
                },
                ctx )
          | P_cons (head, tail) ->
              let s1, ty_head, ctx1 = go head in
              let s2, ty_tail, ctx2 = go tail in
              let list_type = TCustom ("List", [ ty_head.typ ]) in
              let s3 = unify (apply_typ ty_tail.typ s2) list_type in
              ( s3 ++ s2 ++ s1,
                { typ = list_type; pattern = P_T_cons (ty_head, ty_tail) },
                merge_ctx ctx1 ctx2 )
          | P_ctor (name, list) -> (
              let decl =
                let open Canonical.Typedecl in
                List.find_map
                  (fun type_ ->
                    List.find_map
                      (fun ctor ->
                        if name = ctor.id then Some (type_, ctor) else None)
                      type_.ctors)
                  Fake_types.test_custom_definitions
              in
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
                      (Map.empty, Map.empty, [], Map.empty)
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
                      typ = TCustom (type_.name, args);
                      pattern = P_T_ctor (name, List.rev patterns);
                    },
                    ctx )
              | None -> failwith (Printf.sprintf "Unknown constructor %s" name))
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
                Map.empty )
        in
        go pattern
      in
      let fn (patterns, (s0, ty0), (s1, ty1)) { pattern; expr = case_expr } =
        let fn_apply (s2, ty2, ctx') =
          let ctx = Map.union (fun _ _ b -> Some b) ctx ctx' in
          let s3, t3 = infer case_expr (apply_ctx ctx s2) in
          let s4 =
            unify
              (apply_typ ty2.Pattern.Typed.typ (s3 ++ s2))
              (apply_typ ty0 s0)
          in
          let s5 = unify (apply_typ t3 s3) (apply_typ ty1 s1) in
          let s4' = s4 ++ s3 ++ s2 ++ s1 ++ s0 in
          ( ty2 :: patterns,
            (s4', apply_typ ty2.typ s2),
            (s5 ++ s4', apply_typ t3 s5) )
        in
        fn_apply @@ m_pattern pattern
      in
      let patterns, (s1, t1), (s2, ty2) =
        match pattern_data_items with
        | hd :: rest ->
            List.fold_left fn
              (fn ([], (s0, t0), (Map.empty, new_var "a")) hd)
              rest
        | [] -> raise (failwith "no patterns")
      in
      if not @@ Pattern.is_exhaustive (List.rev patterns) then
        failwith "Not exhaustive";
      (s2 ++ s1, ty2)
  | Expr_constr { name; arguments } -> (
      let decl =
        let open Canonical.Typedecl in
        List.find_map
          (fun type_ ->
            List.find_map
              (fun ctor -> if name = ctor.id then Some (type_, ctor) else None)
              type_.ctors)
          Fake_types.test_custom_definitions
      in
      match decl with
      | Some (type_, d) ->
          let s, resolved_types =
            List.fold_left2
              (fun (s_acc, m) (ctor : Canonical.Typedef.Impl.t) arg ->
                let s, ty = infer arg ctx in
                ( s ++ s_acc,
                  match ctor.body with
                  | Canonical.Typedef.Kind.Tkind_var v -> Map.add v.thing ty m
                  | _ -> m ))
              (Map.empty, Map.empty) d.data arguments
          in
          let args =
            List.map
              (fun p ->
                match Map.find_opt p resolved_types with
                | Some t -> t
                | None ->
                    Printf.sprintf "Unknown type variable %s" p |> failwith)
              type_.params
          in
          (s, TCustom (type_.name, args))
      | None -> Printf.sprintf "Unknown constructor %s" name |> failwith)
  | Expr_record_extend label ->
      let a = new_var "a" in
      let r = new_var "r" in
      (Map.empty, TFun (a, TFun (TRecord r, TRecord (TRowExtend (label, a, r)))))
  | Expr_record_empty -> (Map.empty, TRecord TRowEmpty)
  | Expr_record_select label ->
      let a = new_var "a" in
      let r = new_var "r" in
      (Map.empty, TFun (TRecord (TRowExtend (label, a, r)), a))
  | Expr_access { expr; field } ->
      let s1, t1 = infer expr ctx in
      let a = new_var "a" in
      let r = new_var "r" in
      let s2 =
        unify (apply_typ t1 s1) (TRecord (TRowExtend (field.thing, a, r)))
      in
      (s2 ++ s1, apply_typ a s2)
  | Expr_lambda { params; body } ->
      let param_types = List.map (fun _ -> new_var "a") params in
      let ctx_with_params =
        List.fold_left2
          (fun acc param param_ty ->
            Map.add param.Data.Located.thing (Scheme ([], param_ty)) acc)
          ctx params param_types
      in
      let s, ty = infer body ctx_with_params in
      let param_types' = List.map (fun ty -> apply_typ ty s) param_types in
      let fn_ty =
        List.fold_right
          (fun param_ty acc -> TFun (param_ty, acc))
          param_types' ty
      in
      (s, fn_ty)
  | _ -> failwith "Expression not implemented"

let infer_exp exp ctx =
  State.reset ();
  let s, ty = infer exp ctx in
  apply_typ ty s

let infer' = infer
let infer exp = infer_exp exp ctx

let infer_declaration { Canonical.Declaration.body_part; type_part_data } ctx =
  let param_types = List.map (fun _ -> new_var "a") body_part.params in
  let ctx_with_params =
    List.fold_left2
      (fun acc param param_ty ->
        Map.add param.Data.Located.thing (Scheme ([], param_ty)) acc)
      ctx body_part.params param_types
  in

  let s, ty = infer' body_part.expr.Data.Located.thing ctx_with_params in

  let param_types' = List.map (fun ty -> apply_typ ty s) param_types in

  let final_ty =
    List.fold_right
      (fun param_ty acc -> TFun (param_ty, acc))
      param_types' (apply_typ ty s)
  in

  let verified_ty, s_final =
    match type_part_data with
    | None -> (final_ty, s)
    | Some type_part ->
        let declared_ty = typedef_to_type type_part.type_alias.body in
        let s_check = unify final_ty declared_ty in
        (apply_typ final_ty s_check, s_check ++ s)
  in

  let scheme = generalize verified_ty ctx in
  let new_ctx = Map.add body_part.name.thing scheme ctx in

  (s_final, verified_ty, new_ctx)

let infer_toplevel declarations initial_ctx =
  let type_aliases =
    List.fold_left
      (fun acc decl ->
        match decl with
        | Canonical.Impl.Type_alias ta ->
            let alias_type = typedef_to_type ta.typedef.body in
            Map.add ta.name (ta.params, alias_type) acc
        | _ -> acc)
      Map.empty declarations
  in

  let rec expand_type_alias ty =
    match ty with
    | TCustom (name, args) -> (
        match Map.find_opt name type_aliases with
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
                   name (List.length params) (List.length args))
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
          Map.add param.Data.Located.thing (Scheme ([], param_ty)) acc)
        ctx body_part.params param_types
    in

    let s, ty = infer' body_part.expr.Data.Located.thing ctx_with_params in

    let param_types' = List.map (fun ty -> apply_typ ty s) param_types in

    let final_ty =
      List.fold_right
        (fun param_ty acc -> TFun (param_ty, acc))
        param_types' (apply_typ ty s)
    in

    let verified_ty, s_final =
      match type_part_data with
      | None -> (final_ty, s)
      | Some type_part ->
          let declared_ty = typedef_to_type type_part.type_alias.body in
          let expanded_declared_ty = expand_type_alias declared_ty in
          let s_check = unify final_ty expanded_declared_ty in
          (apply_typ final_ty s_check, s_check ++ s)
    in

    let scheme = generalize verified_ty ctx in
    let new_ctx = Map.add body_part.name.thing scheme ctx in

    (s_final, verified_ty, new_ctx)
  in

  let ctx_with_placeholders =
    List.fold_left
      (fun acc decl ->
        match decl with
        | Canonical.Impl.Top_declaration { body_part; type_part_data; _ } ->
            let ty_scheme =
              match type_part_data with
              | Some type_part ->
                  let declared_ty = typedef_to_type type_part.type_alias.body in
                  let expanded_ty = expand_type_alias declared_ty in
                  Scheme ([], expanded_ty)
              | None -> Scheme ([], new_var "a")
            in
            Map.add body_part.name.thing ty_scheme acc
        | Canonical.Impl.Type_alias _ -> acc)
      initial_ctx declarations
  in

  let final_ctx, typed_decls =
    List.fold_left
      (fun (ctx_acc, decls_acc) decl ->
        match decl with
        | Canonical.Impl.Top_declaration decl_data ->
            let s, ty, new_ctx =
              infer_declaration_with_expansion decl_data ctx_acc
            in
            (new_ctx, (decl_data.body_part.name.thing, ty) :: decls_acc)
        | Canonical.Impl.Type_alias _ -> (ctx_acc, decls_acc))
      (ctx_with_placeholders, [])
      declarations
  in

  (final_ctx, List.rev typed_decls)
