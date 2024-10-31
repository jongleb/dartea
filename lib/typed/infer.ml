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
    (* while operators aren't supported *)
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
  ]

let ctx : ctx =
  let f acc (v, scheme) = Map.add v scheme acc in
  List.fold_left f Map.empty typs

open Frontend.Expr
module Str_set = Set.Make (String)

let rec ftv_typ = function
  | TVar v -> Str_set.singleton v
  | TInt | TBool | TStr | TUnit -> Str_set.empty
  | TFun (p, r) -> Str_set.union (ftv_typ p) (ftv_typ r)
  | TTup l ->
      List.fold_left
        (fun acc ty -> Str_set.union (ftv_typ ty) acc)
        Str_set.empty l
  | TCustom (_, typs) ->
      List.fold_left
        (fun acc ty -> Str_set.union (ftv_typ ty) acc)
        Str_set.empty typs

let rec apply_typ ty s =
  match ty with
  | TVar v -> ( match Map.find_opt v s with Some t -> t | None -> ty)
  | TInt | TBool | TStr | TUnit -> ty
  | TFun (p, r) -> TFun (apply_typ p s, apply_typ r s)
  | TTup l -> TTup (List.map (fun ty -> apply_typ ty s) l)
  | TCustom (name, typs) ->
      TCustom (name, List.map (fun ty -> apply_typ ty s) typs)

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

let unify ty1 ty2 =
  let unify_err ty1 ty2 =
    let ty1' = string_of_typ ty1 and ty2' = string_of_typ ty2 in
    Printf.sprintf "Unification failed for %s and %s" ty1' ty2' |> failwith
  in
  let rec unify' = function
    | TVar v, ty | ty, TVar v -> bind_var ty v
    | TInt, TInt -> Map.empty
    | TStr, TStr -> Map.empty
    | TBool, TBool -> Map.empty
    | TUnit, TUnit -> Map.empty
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
    | ty1, ty2 -> unify_err ty1 ty2
  in

  unify' (ty1, ty2)

let istantiate = function
  | Scheme (vars, ty) ->
      let vars' = vars |> List.map (fun _ -> new_var "a") in
      List.combine vars vars' |> List.to_seq |> Map.of_seq |> apply_typ ty

let rec typedef_to_type =
  Frontend.Typedef.(
    Kind.(
      Impl.(
        function
        | { body = Tkind_var _; parameters = [] } -> new_var "a"
        | { body = Tkind_concrete { thing = "Int" }; parameters = [] } -> TInt
        | { body = Tkind_concrete { thing = "String" }; parameters = [] } ->
            TStr
        | { body = Tkind_concrete { thing }; parameters } ->
            TCustom (thing, List.map typedef_to_type parameters)
        | _ -> assert false)))

let merge_ctx = Map.union (fun _key _val1 _val2 -> failwith "Ambigous")

let rec infer exp ctx =
  match exp with
  | Expr_int _ -> (Map.empty, TInt)
  | Expr_string _ -> (Map.empty, TStr)
  | Expr_let { binding = { bind_body; _ }; body } ->
      let s1, ty1 = infer bind_body.body ctx in
      let ctx1 = Map.remove bind_body.name.thing ctx in
      let scheme = apply_ctx ctx1 s1 |> generalize ty1 in
      let ctx2 = Map.add bind_body.name.thing scheme ctx1 in
      let s2, ty2 = infer body (apply_ctx ctx2 s1) in
      (s2 ++ s1, ty2)
  | Expr_apply { fn; arg } ->
      let s1, ty = infer fn ctx in
      let s2, p = infer arg (apply_ctx ctx s1) in
      let r = new_var "a" in
      let s3 = unify (apply_typ ty s2) (TFun (p, r)) in
      (s3 ++ s2 ++ s1, apply_typ r s3)
  | Expr_ident v -> (
      match Map.find_opt v ctx with
      | Some scheme -> (Map.empty, istantiate scheme)
      | None -> Printf.sprintf "Unbound variable %s" v |> failwith)
  | Expr_if_then_else { if_exp; then_exp; else_exp } ->
      let s1, cond = infer if_exp ctx in
      let s2, ty1 = infer then_exp (apply_ctx ctx s1) in
      let s3, ty2 = infer else_exp (apply_ctx ctx s1) in
      let s4 = unify (apply_typ cond s1) TBool in
      let s5 = unify (apply_typ ty1 s3) (apply_typ ty2 s3) in
      (s5 ++ s4 ++ s3 ++ s2 ++ s1, apply_typ ty1 s4)
  | Expr_pattern { expr; pattern_data_items } ->
      let s0, match_ = infer expr ctx in
      let rec m_pattern =
        Pattern.Typed.(
          function
          | Frontend.Pattern.P_str s ->
              (Map.empty, { typ = TStr; pattern = P_T_str s }, Map.empty)
          | P_int i ->
              (Map.empty, { typ = TInt; pattern = P_T_int i }, Map.empty)
          | P_anything ->
              ( Map.empty,
                { typ = new_var "a"; pattern = P_T_anything },
                Map.empty )
          | P_var name ->
              let typ = new_var "a" in
              let ctx1 = Map.remove name ctx in
              let scheme = Type.Scheme ([], typ) in
              let ctx2 = Map.add name scheme ctx1 in
              (Map.empty, { typ; pattern = P_T_var name }, ctx2)
          | P_tuple list ->
              let map, typs, pats, ctx =
                List.fold_right
                  (fun pat (fv, typs, pats, ctx_map) ->
                    let map_r, pat, ctx = m_pattern pat in
                    ( map_r ++ fv,
                      pat.typ :: typs,
                      pat :: pats,
                      merge_ctx ctx ctx_map ))
                  list
                  (Map.empty, [], [], Map.empty)
              in
              ( map,
                { typ = TTup (List.rev typs); pattern = P_T_tuple pats },
                ctx )
          | P_ctor (name, list) -> (
              let decl =
                let open Frontend.Typedecl in
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
                           (pattern : Frontend.Pattern.t)
                           (ctor : Frontend.Typedef.Impl.t) ->
                        let s, ty_arg, ctx' = m_pattern pattern in
                        (* let ty_def = typedef_to_type ctor in
                           let s2 =
                             unify (apply_typ ty_arg s) (apply_typ ty_def s)
                           in *)
                        ( (* acc ++ s ++ s2, *)
                          acc ++ s,
                          (match ctor.body with
                          | Frontend.Typedef.Kind.Tkind_var v ->
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
              | None -> assert false)
          | _ -> assert false)
      in
      let fn (patterns, (s0, ty0), (s1, ty1)) { pattern; expr = case_expr } =
        let fn_apply (s2, ty2, ctx') =
          let ctx = Map.union (fun _ _ b -> Some b) ctx ctx' in
          let s3, ty3 = infer case_expr (apply_ctx ctx s2) in
          let s4 =
            unify
              (apply_typ ty2.Pattern.Typed.typ (s3 ++ s2))
              (apply_typ ty0 s0)
          in
          let s5 = unify (apply_typ ty3 s3) (apply_typ ty1 s1) in
          let s4' = s4 ++ s3 ++ s2 ++ s1 ++ s0 in
          ( ty2 :: patterns,
            (s4', apply_typ ty2.typ s2),
            (s5 ++ s4', apply_typ ty3 s5) )
        in
        pattern |> m_pattern |> fn_apply
      in
      let patterns, (s1, t1), (s2, ty2) =
        match pattern_data_items with
        | hd :: rest ->
            List.fold_left fn
              (fn ([], (s0, match_), (Map.empty, new_var "a")) hd)
              rest
        | [] -> raise (failwith "no patterns")
      in
      (* TODO FIX ME *)
      if not @@ Pattern.is_exhaustive (List.rev patterns) then
        failwith "Not exhaustive";
      (s2 ++ s1, ty2)
  | Expr_constr { name; arguments } -> (
      let decl =
        let open Frontend.Typedecl in
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
              (fun (acc, m) (ctor : Frontend.Typedef.Impl.t) arg ->
                let s, ty_arg = infer arg ctx in
                (* let ty_def = typedef_to_type ctor in *)
                (* let s2 = unify (apply_typ ty_arg s) (apply_typ ty_def s) in *)
                ( acc ++ s,
                  (* acc ++ s ++ s2, *)
                  match ctor.body with
                  | Frontend.Typedef.Kind.Tkind_var v ->
                      Map.add v.thing ty_arg m
                  | Tkind_concrete _ | Tkind_record _ | Tkind_tuple _
                  | Tkind_function _ | Tkind_unit ->
                      m ))
              (Map.empty, Map.empty) d.data arguments
          in
          let args =
            List.map
              (fun p ->
                match Map.find_opt p resolved_types with
                | Some t -> t
                | None ->
                    (* get rid of it *)
                    Printf.sprintf "Unknown type variable %s" p |> failwith)
              type_.params
          in
          (s, TCustom (type_.name, args))
      | None -> Printf.sprintf "Unknown constructor %s" name |> failwith)
  | _ -> assert false

let infer_exp exp ctx =
  State.reset ();
  let s, ty = infer exp ctx in
  apply_typ ty s

let infer' = infer
let infer exp = infer_exp exp ctx
