module Ast = struct
  type typ =
    | TVar of string
    | TInt
    | TBool
    | TUnit
    | TFun of typ * typ
    | TTup of typ list
  [@@deriving show]

  type scheme = Scheme of string list * typ [@@deriving show]
end

open Ast
module Map = Map.Make (String)

type ctx = scheme Map.t

let ctx : ctx =
  let typs : (string * scheme) list =
    [
      ("=", Scheme ([ "'a" ], TFun (TVar "'a", TFun (TVar "'a", TBool))));
      ("<>", Scheme ([ "'a" ], TFun (TVar "'a", TFun (TVar "'a", TBool))));
      ("&&", Scheme ([], TFun (TBool, TFun (TBool, TBool))));
      ("||", Scheme ([], TFun (TBool, TFun (TBool, TBool))));
      ("+", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
      ("plus", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
      (* while operators aren't supported *)
      ("-", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
      ("*", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
      ("/", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
      ("id", Scheme ([ "'a" ], TFun (TVar "'a", TVar "'a")));
      ( "const",
        Scheme ([ "'a"; "'b" ], TFun (TVar "'a", TFun (TVar "'b", TVar "'a")))
      );
      ( "pair",
        Scheme
          ( [ "'a"; "'b" ],
            TFun (TVar "'a", TFun (TVar "'b", TTup [ TVar "'a"; TVar "'b" ])) )
      );
      ( "fst",
        Scheme ([ "'a"; "'b" ], TFun (TTup [ TVar "'a"; TVar "'b" ], TVar "'a"))
      );
      ( "snd",
        Scheme ([ "'a"; "'b" ], TFun (TTup [ TVar "'a"; TVar "'b" ], TVar "'b"))
      );
    ]
  in
  let f acc (v, scheme) = Map.add v scheme acc in
  List.fold_left f Map.empty typs

open Frontend.Expr
module Set = Set.Make (String)

let rec ftv_typ = function
  | TVar v -> Set.singleton v
  | TInt | TBool | TUnit -> Set.empty
  | TFun (p, r) -> Set.union (ftv_typ p) (ftv_typ r)
  | TTup l ->
      List.fold_left (fun acc ty -> Set.union (ftv_typ ty) acc) Set.empty l

let rec apply_typ ty s =
  match ty with
  | TVar v -> ( match Map.find_opt v s with Some t -> t | None -> ty)
  | TInt | TBool | TUnit -> ty
  | TFun (p, r) -> TFun (apply_typ p s, apply_typ r s)
  | TTup l -> TTup (List.map (fun ty -> apply_typ ty s) l)

let string_of_typ ty =
  let rec str_simple ty =
    match ty with
    | TVar v -> v
    | TInt -> "int"
    | TBool -> "bool"
    | TUnit -> "unit"
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
  | Scheme (vars, ty) -> Set.diff (ftv_typ ty) (Set.of_list vars)

let ftv_ctx ctx =
  Map.fold (fun _ scheme acc -> Set.union acc (ftv_scheme scheme)) ctx Set.empty

let generalize ty ctx =
  let vars = Set.diff (ftv_typ ty) (ftv_ctx ctx) |> Set.to_seq in
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
      if Set.mem v (ftv_typ ty) then
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
    | ty1, ty2 -> unify_err ty1 ty2
  in
  unify' (ty1, ty2)

let istantiate = function
  | Scheme (vars, ty) ->
      let vars' = vars |> List.map (fun _ -> new_var "a") in
      List.combine vars vars' |> List.to_seq |> Map.of_seq |> apply_typ ty

let rec infer exp ctx =
  match exp with
  | Expr_int _ -> (Map.empty, TInt)
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
  | _ -> assert false

(**
             Например вывод для let a = 2 in a + 1

             print_endline (Printf.sprintf "r: %s" (Ast.show_typ r));
             print_endline (Printf.sprintf "apply_typ ty s2: %s" (Ast.show_typ (apply_typ ty s2)));
             print_endline (Printf.sprintf "TFun (p, r) ty s2: %s" (Ast.show_typ (TFun (p, r))));

             Let a (Int 2) (App (App (Var +) (Var a)) (Int 1))
             r: (Ast.TVar "a0")
             apply_typ ty s2: (Ast.TFun (Ast.TInt, (Ast.TFun (Ast.TInt, Ast.TInt))))
             TFun (p, r) ty s2: (Ast.TFun (Ast.TInt, (Ast.TVar "a0")))
             r: (Ast.TVar "a1")
             apply_typ ty s2: (Ast.TFun (Ast.TInt, Ast.TInt))
             TFun (p, r) ty s2: (Ast.TFun (Ast.TInt, (Ast.TVar "a1")))

         *)

let infer_exp exp ctx =
  State.reset ();
  let s, ty = infer exp ctx in
  apply_typ ty s

let infer exp = infer_exp exp ctx
