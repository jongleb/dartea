open Type
module Map = Map.Make (String)

type ctx = scheme Map.t

(* Field constraints для отложенного разрешения типов записей *)
type field_constraint = {
  field_name : string;
  field_type : t;
  record_var : string;
}

(* Результат инференса с constraints *)
type infer_result = {
  subst : t Map.t;
  ty : t;
  constraints : field_constraint list;
}

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
  | TClosedRecord fields ->
      List.fold_left
        (fun acc (_, ty) -> Str_set.union (ftv_typ ty) acc)
        Str_set.empty fields
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
  | TClosedRecord fields ->
      TClosedRecord (List.map (fun (name, ty) -> (name, apply_typ ty s)) fields)
  | TRowExtend (l, t, r) -> TRowExtend (l, apply_typ t s, apply_typ r s)

let string_of_typ ty =
  let rec str_simple ty =
    match ty with
    | TVar v -> v
    | TInt -> "int"
    | TBool -> "bool"
    | TUnit -> "unit"
    | TStr -> "string"
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
        Buffer.to_bytes buf |> Bytes.to_string
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
    | TRecord r -> Printf.sprintf "{ %s }" (str_simple r)
    | TClosedRecord fields ->
        let fields_str =
          List.map
            (fun (name, ty) -> Printf.sprintf "%s: %s" name (str_simple ty))
            fields
          |> String.concat ", "
        in
        Printf.sprintf "{| %s |}" fields_str
    | TRowExtend (label, typ, row) ->
        Printf.sprintf "%s = %s | %s" label (str_simple typ) (str_simple row)
    | TRowEmpty -> "{}"
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

let rec unify_closed_records fields1 fields2 =
  let sorted1 =
    List.sort (fun (n1, _) (n2, _) -> String.compare n1 n2) fields1
  in
  let sorted2 =
    List.sort (fun (n1, _) (n2, _) -> String.compare n1 n2) fields2
  in

  if List.length sorted1 <> List.length sorted2 then
    failwith "Closed records have different number of fields"
  else
    List.fold_left2
      (fun acc (name1, ty1) (name2, ty2) ->
        if name1 <> name2 then
          failwith
            (Printf.sprintf "Field names don't match: %s vs %s" name1 name2)
        else
          let s = unify ty1 ty2 in
          s ++ acc)
      Map.empty sorted1 sorted2

and find_field_in_closed_record fields field_name =
  List.find_opt (fun (name, _) -> name = field_name) fields

and unify ty1 ty2 =
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
    (* Унификация двух закрытых записей *)
    | TClosedRecord fields1, TClosedRecord fields2 ->
        unify_closed_records fields1 fields2
    (* Закрытая запись не может унифицироваться с row poly *)
    | TClosedRecord _, TRecord _ | TRecord _, TClosedRecord _ ->
        unify_err ty1 ty2
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
        let field_list =
          List.map
            (fun (row : Type_record_row.t) ->
              (row.name.thing, convert row.body.body))
            fields.values
        in
        TClosedRecord field_list
  in
  convert typedef

let rec infer (exp : Canonical.Expr.t) ctx : infer_result =
  match exp with
  | Expr_int _ -> { subst = Map.empty; ty = TInt; constraints = [] }
  | Expr_float _ -> { subst = Map.empty; ty = TInt; constraints = [] }
  | Expr_string _ -> { subst = Map.empty; ty = TStr; constraints = [] }
  | Expr_char _ -> { subst = Map.empty; ty = TStr; constraints = [] }
  | Expr_ident v -> (
      match Map.find_opt v ctx with
      | Some s ->
          let _, ty = instantiate s in
          { subst = Map.empty; ty; constraints = [] }
      | None -> Printf.sprintf "Unbound value %s" v |> failwith)
  | Expr_apply { fn; arg } ->
      let r1 = infer fn ctx in
      let r2 = infer arg (apply_ctx ctx r1.subst) in
      let ty_res = new_var "a" in
      let s3 = unify (apply_typ r1.ty r2.subst) (TFun (r2.ty, ty_res)) in
      let final_subst = s3 ++ r2.subst ++ r1.subst in
      {
        subst = final_subst;
        ty = apply_typ ty_res s3;
        constraints = r1.constraints @ r2.constraints;
      }
  | Expr_if_then_else { if_exp; then_exp; else_exp } ->
      let r1 = infer if_exp ctx in
      let s2 = unify (apply_typ r1.ty r1.subst) TBool in
      let r3 = infer then_exp (apply_ctx ctx (s2 ++ r1.subst)) in
      let r4 = infer else_exp (apply_ctx ctx (r3.subst ++ s2 ++ r1.subst)) in
      let s5 = unify (apply_typ r3.ty r4.subst) r4.ty in
      let final_subst = s5 ++ r4.subst ++ r3.subst ++ s2 ++ r1.subst in
      {
        subst = final_subst;
        ty = apply_typ r4.ty s5;
        constraints = r1.constraints @ r3.constraints @ r4.constraints;
      }
  | Expr_list l ->
      let elem_type = new_var "a" in
      let results =
        List.fold_left
          (fun acc expr ->
            let r = infer expr (apply_ctx ctx acc.subst) in
            let s_unify = unify (apply_typ elem_type acc.subst) r.ty in
            {
              subst = s_unify ++ r.subst ++ acc.subst;
              ty = apply_typ elem_type s_unify;
              constraints = r.constraints @ acc.constraints;
            })
          { subst = Map.empty; ty = elem_type; constraints = [] }
          l
      in
      { results with ty = TCustom ("List", [ results.ty ]) }
  | Expr_let { binding = { bind_body = { name; body = rhs } }; body } ->
      let r1 = infer rhs ctx in
      let ctx' = apply_ctx ctx r1.subst in
      let gen_ty = generalize r1.ty ctx' in
      let ctx'' = Map.add name.thing gen_ty ctx' in
      let r2 = infer body ctx'' in
      {
        subst = r2.subst ++ r1.subst;
        ty = r2.ty;
        constraints = r1.constraints @ r2.constraints;
      }
  | Expr_pattern { expr; pattern_data_items } ->
      let r0 = infer expr ctx in
      let m_pattern (pattern : Canonical.Pattern.t) =
        let rec go = function
          | Canonical.Pattern.P_var v ->
              ( Map.empty,
                { Pattern.Typed.typ = new_var "a"; pattern = P_T_var v },
                Map.singleton v (Scheme ([], new_var "a")),
                [] )
          | P_anything ->
              ( Map.empty,
                { typ = new_var "a"; pattern = P_T_anything },
                Map.empty,
                [] )
          | P_int i ->
              (Map.empty, { typ = TInt; pattern = P_T_int i }, Map.empty, [])
          | P_str s ->
              (Map.empty, { typ = TStr; pattern = P_T_str s }, Map.empty, [])
          | P_chr c ->
              (Map.empty, { typ = TStr; pattern = P_T_chr c }, Map.empty, [])
          | P_unit ->
              (Map.empty, { typ = TUnit; pattern = P_T_unit }, Map.empty, [])
          | P_tuple list ->
              let s, resolved, ctx, constrs =
                List.fold_left
                  (fun (s_acc, res_acc, ctx_acc, constr_acc) pat ->
                    let s, ty, ctx', constrs = go pat in
                    ( s ++ s_acc,
                      ty :: res_acc,
                      merge_ctx ctx_acc ctx',
                      constrs @ constr_acc ))
                  (Map.empty, [], Map.empty, [])
                  list
              in
              ( s,
                {
                  typ =
                    TTup (List.rev_map (fun t -> t.Pattern.Typed.typ) resolved);
                  pattern = P_T_tuple (List.rev resolved);
                },
                ctx,
                constrs )
          | P_list list ->
              let elem_type = new_var "a" in
              let s, resolved, ctx, constrs =
                List.fold_left
                  (fun (s_acc, res_acc, ctx_acc, constr_acc) pat ->
                    let s, ty, ctx', constrs = go pat in
                    let s_unify = unify (apply_typ elem_type s) ty.typ in
                    ( s_unify ++ s ++ s_acc,
                      ty :: res_acc,
                      merge_ctx ctx_acc ctx',
                      constrs @ constr_acc ))
                  (Map.empty, [], Map.empty, [])
                  list
              in
              ( s,
                {
                  typ = TCustom ("List", [ elem_type ]);
                  pattern = P_T_list (List.rev resolved);
                },
                ctx,
                constrs )
          | P_cons (head, tail) ->
              let s1, ty_head, ctx1, constrs1 = go head in
              let s2, ty_tail, ctx2, constrs2 = go tail in
              let list_type = TCustom ("List", [ ty_head.typ ]) in
              let s3 = unify (apply_typ ty_tail.typ s2) list_type in
              ( s3 ++ s2 ++ s1,
                { typ = list_type; pattern = P_T_cons (ty_head, ty_tail) },
                merge_ctx ctx1 ctx2,
                constrs1 @ constrs2 )
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
                  let s, resolved_types, patterns, ctx, constrs =
                    List.fold_left2
                      (fun (acc, m, patterns, ctx, constr_acc)
                           (pattern : Canonical.Pattern.t)
                           (ctor : Canonical.Typedef.Impl.t) ->
                        let s, ty_arg, ctx', constrs = go pattern in
                        ( acc ++ s,
                          (match ctor.body with
                          | Canonical.Typedef.Kind.Tkind_var v ->
                              Map.add v.thing ty_arg.typ m
                          | _ -> m),
                          ty_arg :: patterns,
                          merge_ctx ctx ctx',
                          constrs @ constr_acc ))
                      (Map.empty, Map.empty, [], Map.empty, [])
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
                    ctx,
                    constrs )
              | None -> failwith (Printf.sprintf "Unknown constructor %s" name))
          | P_record fields ->
              (* Для записей создаем row poly тип *)
              let field_type = new_var "a" in
              let row_var = new_var "r" in
              let row_type =
                List.fold_right
                  (fun field_name acc ->
                    TRowExtend (field_name, field_type, acc))
                  fields row_var
              in
              ( Map.empty,
                { typ = TRecord row_type; pattern = P_T_record fields },
                Map.empty,
                [] )
        in
        go pattern
      in
      let fn (patterns, (s0, ty0), (s1, ty1), all_constrs)
          { pattern; expr = case_expr } =
        let fn_apply (s2, ty2, ctx', constrs) =
          let ctx = Map.union (fun _ _ b -> Some b) ctx ctx' in
          let r3 = infer case_expr (apply_ctx ctx s2) in
          let s4 =
            unify
              (apply_typ ty2.Pattern.Typed.typ (r3.subst ++ s2))
              (apply_typ ty0 s0)
          in
          let s5 = unify (apply_typ r3.ty r3.subst) (apply_typ ty1 s1) in
          let s4' = s4 ++ r3.subst ++ s2 ++ s1 ++ s0 in
          ( ty2 :: patterns,
            (s4', apply_typ ty2.typ s2),
            (s5 ++ s4', apply_typ r3.ty s5),
            constrs @ r3.constraints @ all_constrs )
        in
        fn_apply @@ m_pattern pattern
      in
      let patterns, (s1, t1), (s2, ty2), all_constrs =
        match pattern_data_items with
        | hd :: rest ->
            List.fold_left fn
              (fn
                 ( [],
                   (r0.subst, r0.ty),
                   (Map.empty, new_var "a"),
                   r0.constraints )
                 hd)
              rest
        | [] -> raise (failwith "no patterns")
      in
      if not @@ Pattern.is_exhaustive (List.rev patterns) then
        failwith "Not exhaustive";
      { subst = s2 ++ s1; ty = ty2; constraints = all_constrs }
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
          let results, resolved_types =
            List.fold_left2
              (fun (acc, m) (ctor : Canonical.Typedef.Impl.t) arg ->
                let r = infer arg ctx in
                ( {
                    subst = r.subst ++ acc.subst;
                    ty = acc.ty;
                    constraints = r.constraints @ acc.constraints;
                  },
                  match ctor.body with
                  | Canonical.Typedef.Kind.Tkind_var v -> Map.add v.thing r.ty m
                  | _ -> m ))
              ({ subst = Map.empty; ty = TUnit; constraints = [] }, Map.empty)
              d.data arguments
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
          { results with ty = TCustom (type_.name, args) }
      | None -> Printf.sprintf "Unknown constructor %s" name |> failwith)
  | Expr_record_extend label ->
      let a = new_var "a" in
      let r = new_var "r" in
      {
        subst = Map.empty;
        ty = TFun (a, TFun (TRecord r, TRecord (TRowExtend (label, a, r))));
        constraints = [];
      }
  | Expr_record_empty ->
      { subst = Map.empty; ty = TRecord TRowEmpty; constraints = [] }
  | Expr_record_select label ->
      let a = new_var "a" in
      let r = new_var "r" in
      {
        subst = Map.empty;
        ty = TFun (TRecord (TRowExtend (label, a, r)), a);
        constraints = [];
      }
  | Expr_access { expr; field } -> (
      let r1 = infer expr ctx in
      match apply_typ r1.ty r1.subst with
      | TClosedRecord fields -> (
          match find_field_in_closed_record fields field.thing with
          | Some (_, field_type) ->
              {
                subst = r1.subst;
                ty = field_type;
                constraints = r1.constraints;
              }
          | None ->
              Printf.sprintf "Field %s not found in closed record" field.thing
              |> failwith)
      | TRecord row ->
          let a = new_var "a" in
          let r = new_var "r" in
          let s2 =
            unify (apply_typ r1.ty r1.subst)
              (TRecord (TRowExtend (field.thing, a, r)))
          in
          {
            subst = s2 ++ r1.subst;
            ty = apply_typ a s2;
            constraints = r1.constraints;
          }
      | TVar var_name ->
          let field_type = new_var "a" in
          let new_constraint =
            { field_name = field.thing; field_type; record_var = var_name }
          in
          {
            subst = r1.subst;
            ty = field_type;
            constraints = new_constraint :: r1.constraints;
          }
      | _ ->
          Printf.sprintf "Cannot access field %s on non-record type %s"
            field.thing (string_of_typ r1.ty)
          |> failwith)
  | Expr_lambda { params; body } ->
      let param_types = List.map (fun _ -> new_var "a") params in
      let ctx_with_params =
        List.fold_left2
          (fun acc param param_ty ->
            Map.add param.Data.Located.thing (Scheme ([], param_ty)) acc)
          ctx params param_types
      in
      let r = infer body ctx_with_params in
      let param_types' =
        List.map (fun ty -> apply_typ ty r.subst) param_types
      in
      let fn_ty =
        List.fold_right
          (fun param_ty acc -> TFun (param_ty, acc))
          param_types' r.ty
      in
      { subst = r.subst; ty = fn_ty; constraints = r.constraints }
  | _ -> failwith "Expression not implemented"

(* Разрешение field constraints после основного инференса *)
let resolve_field_constraints s ty constraints =
  List.fold_left
    (fun (s_acc, ty_acc) constr ->
      (* Находим актуальный тип переменной записи *)
      let resolved_type = apply_typ (TVar constr.record_var) s_acc in

      match resolved_type with
      (* Переменная все еще свободна - делаем row poly *)
      | TVar _ ->
          let rest = new_var "r" in
          let row_type =
            TRecord (TRowExtend (constr.field_name, constr.field_type, rest))
          in
          let s_new = Map.add constr.record_var row_type s_acc in
          (s_new, apply_typ ty_acc s_new)
      (* Уже закрытая запись - проверяем совместимость *)
      | TClosedRecord fields -> (
          match find_field_in_closed_record fields constr.field_name with
          | Some (_, field_type) ->
              let s_unify = unify field_type constr.field_type in
              (s_unify ++ s_acc, apply_typ ty_acc s_unify)
          | None ->
              Printf.sprintf "Field %s not found in closed record"
                constr.field_name
              |> failwith)
      (* Уже row poly - добавляем constraint *)
      | TRecord row ->
          let s_unify =
            unify row
              (TRowExtend (constr.field_name, constr.field_type, new_var "r"))
          in
          (s_unify ++ s_acc, apply_typ ty_acc s_unify)
      | _ -> (s_acc, ty_acc))
    (s, ty) constraints

let infer_exp exp ctx =
  State.reset ();
  let result = infer exp ctx in
  let s_final, ty_final =
    resolve_field_constraints result.subst result.ty result.constraints
  in
  apply_typ ty_final s_final

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

  let result = infer' body_part.expr.Data.Located.thing ctx_with_params in

  let param_types' =
    List.map (fun ty -> apply_typ ty result.subst) param_types
  in

  let final_ty =
    List.fold_right
      (fun param_ty acc -> TFun (param_ty, acc))
      param_types'
      (apply_typ result.ty result.subst)
  in

  let verified_ty, s_final =
    match type_part_data with
    | None -> (final_ty, result.subst)
    | Some type_part ->
        let declared_ty = typedef_to_type type_part.type_alias.body in
        let s_check = unify final_ty declared_ty in
        (apply_typ final_ty s_check, s_check ++ result.subst)
  in

  let scheme = generalize verified_ty ctx in
  let new_ctx = Map.add body_part.name.thing scheme ctx in

  (s_final, verified_ty, new_ctx)

let infer_toplevel declarations initial_ctx =
  let ctx_with_names =
    List.fold_left
      (fun acc decl ->
        match decl with
        | Canonical.Impl.Top_declaration { body_part; _ } ->
            let fresh_ty = new_var "a" in
            let scheme = Scheme ([], fresh_ty) in
            Map.add body_part.name.thing scheme acc)
      initial_ctx declarations
  in

  let inferred_types =
    List.filter_map
      (fun decl ->
        match decl with
        | Canonical.Impl.Top_declaration decl_data ->
            let s, ty, _ = infer_declaration decl_data ctx_with_names in
            Some (decl_data.body_part.name.thing, (s, ty)))
      declarations
  in

  let final_ctx =
    List.fold_left
      (fun acc (name, (s, inferred_ty)) ->
        match Map.find_opt name ctx_with_names with
        | Some (Scheme ([], fresh_var)) ->
            let s_unify = unify fresh_var inferred_ty in
            let final_ty = apply_typ inferred_ty s_unify in
            let scheme = generalize final_ty initial_ctx in
            Map.add name scheme acc
        | _ -> acc)
      initial_ctx inferred_types
  in

  let typed_decls =
    List.map
      (fun (name, (_, ty)) ->
        match Map.find_opt name final_ctx with
        | Some scheme ->
            (name, snd (match scheme with Scheme (_, t) -> ([], t)))
        | None -> (name, ty))
      inferred_types
  in

  (final_ctx, typed_decls)
