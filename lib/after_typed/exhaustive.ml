module P = Optimized.Pattern

module Occurrence = struct
  type step = Payload of int | Index of int | Field of string | Hd | Tl
  type t = step list

  let root : t = []
  let child o s = o @ [ s ]
end

module Decision_tree = struct
  type test =
    | Test_ctor of Data.Name.t
    | Test_tag of Data.Name.t
    | Test_int of int
    | Test_str of string
    | Test_chr of string
    | Test_nil
    | Test_cons

  type t =
    | Fail
    | Leaf of { action : int; bindings : (string * Occurrence.t) list }
    | Switch of {
        occurrence : Occurrence.t;
        branches : (test * t) list;
        default : t option;
      }
end

open Occurrence
open Decision_tree

type head =
  | H_ctor of Data.Name.t
  | H_int of int
  | H_str of string
  | H_chr of string
  | H_nil
  | H_cons

type shape =
  | Wild
  | Var of string
  | Tuple of P.t list
  | Record of string list
  | Head of head * P.t list

let rec peeled p =
  match p.P.pattern with
  | P.P_T_alias (inner, _) -> peeled inner
  | P.P_T_anything | P.P_T_var _ | P.P_T_record _ | P.P_T_unit
  | P.P_T_tuple _ | P.P_T_list _ | P.P_T_cons _ | P.P_T_chr _ | P.P_T_str _
  | P.P_T_int _ | P.P_T_ctor _ ->
      p

let rec alias_names p =
  match p.P.pattern with
  | P.P_T_alias (inner, name) -> name :: alias_names inner
  | P.P_T_anything | P.P_T_var _ | P.P_T_record _ | P.P_T_unit
  | P.P_T_tuple _ | P.P_T_list _ | P.P_T_cons _ | P.P_T_chr _ | P.P_T_str _
  | P.P_T_int _ | P.P_T_ctor _ ->
      []

let shape_of p =
  match (peeled p).P.pattern with
  | P.P_T_anything | P.P_T_unit -> Wild
  | P.P_T_var x -> Var x
  | P.P_T_tuple ps -> Tuple ps
  | P.P_T_record fields -> Record fields
  | P.P_T_ctor (n, args) -> Head (H_ctor n, args)
  | P.P_T_int i -> Head (H_int i, [])
  | P.P_T_str s -> Head (H_str s, [])
  | P.P_T_chr c -> Head (H_chr c, [])
  | P.P_T_cons (hd, tl) -> Head (H_cons, [ hd; tl ])
  | P.P_T_list [] -> Head (H_nil, [])
  | P.P_T_list (x :: xs) ->
      Head (H_cons, [ x; { p with P.pattern = P.P_T_list xs } ])
  | P.P_T_alias _ -> Wild

let head_of p =
  match shape_of p with
  | Head (h, _) -> Some h
  | Wild | Var _ | Tuple _ | Record _ -> None

let subpats p =
  match shape_of p with
  | Head (_, subs) -> subs
  | Wild | Var _ | Tuple _ | Record _ -> []

let is_wild p =
  match shape_of p with
  | Wild | Var _ -> true
  | Tuple _ | Record _ | Head _ -> false

let wildcard = { P.typ = Optimized.Type.TUnit; pattern = P.P_T_anything }
let wildcards n = List.init n (fun _ -> wildcard)
let pat k = { P.typ = Optimized.Type.TUnit; pattern = k }

let head_pattern h subs =
  let nth n = Option.value (List.nth_opt subs n) ~default:wildcard in
  match h with
  | H_ctor n -> pat (P.P_T_ctor (n, subs))
  | H_int i -> pat (P.P_T_int i)
  | H_str s -> pat (P.P_T_str s)
  | H_chr c -> pat (P.P_T_chr c)
  | H_nil -> pat (P.P_T_list [])
  | H_cons -> pat (P.P_T_cons (nth 0, nth 1))

let tuple_pattern subs = pat (P.P_T_tuple subs)

let split_at n lst =
  let rec go acc n = function
    | xs when n <= 0 -> (List.rev acc, xs)
    | [] -> (List.rev acc, [])
    | x :: xs -> go (x :: acc) (n - 1) xs
  in
  go [] n lst

let column rows = List.filter_map (function p :: _ -> Some p | [] -> None) rows

let signatures column =
  let rec go seen acc = function
    | [] -> List.rev acc
    | p :: rest -> begin
        match shape_of p with
        | Head (h, subs) when not (List.mem h seen) ->
            go (h :: seen) ((h, subs) :: acc) rest
        | Head _ | Wild | Var _ | Tuple _ | Record _ -> go seen acc rest
      end
  in
  go [] [] column

let tuple_arity column =
  let arity_of p =
    match shape_of p with
    | Tuple ps -> Some (List.length ps)
    | Wild | Var _ | Record _ | Head _ -> None
  in
  List.find_map arity_of column

let complete siblings sigs =
  match sigs with
  | [] -> false
  | (h, _) :: _ -> begin
      match h with
      | H_int _ | H_str _ | H_chr _ -> false
      | H_nil | H_cons ->
          List.exists (fun (x, _) -> x = H_nil) sigs
          && List.exists (fun (x, _) -> x = H_cons) sigs
      | H_ctor n -> begin
          match siblings n with
          | Some sibs -> List.length sigs = List.length sibs
          | None -> false
        end
    end

let specialize_ctor ~head:want ~arity rows =
  List.filter_map
    (fun row ->
      match row with
      | [] -> None
      | front :: rest -> begin
          match head_of front with
          | None -> Some (wildcards arity @ rest)
          | Some h -> if h = want then Some (subpats front @ rest) else None
        end)
    rows

let specialize_tuple ~arity rows =
  List.filter_map
    (fun row ->
      match row with
      | [] -> None
      | front :: rest -> begin
          match shape_of front with
          | Tuple ps -> Some (ps @ rest)
          | Wild | Var _ | Record _ | Head _ ->
              Some (wildcards arity @ rest)
        end)
    rows

let default_pats rows =
  List.filter_map
    (function front :: rest when head_of front = None -> Some rest | _ -> None)
    rows

let missing_witness siblings sigs =
  let has h = List.exists (fun (x, _) -> x = h) sigs in
  match sigs with
  | [] -> None
  | (H_nil, _) :: _ | (H_cons, _) :: _ ->
      if not (has H_cons) then Some (head_pattern H_cons (wildcards 2))
      else if not (has H_nil) then Some (head_pattern H_nil [])
      else None
  | (H_ctor n, _) :: _ -> begin
      match siblings n with
      | None -> None
      | Some sibs ->
          List.find_opt (fun (name, _) -> not (has (H_ctor name))) sibs
          |> Option.map (fun (name, arity) ->
                 head_pattern (H_ctor name) (wildcards arity))
    end
  | (H_int _, _) :: _ ->
      let taken =
        let taken_int (x, _) =
          match x with
          | H_int i -> Some i
          | H_ctor _ | H_str _ | H_chr _ | H_nil | H_cons -> None
        in
        List.filter_map taken_int sigs
      in
      let rec pick n = if List.mem n taken then pick (n + 1) else n in
      Some (head_pattern (H_int (pick 0)) [])
  | (H_str _, _) :: _ | (H_chr _, _) :: _ -> None

let rec useful siblings rows q =
  match q with
  | [] -> if rows = [] then Some [] else None
  | q1 :: qrest -> begin
      match shape_of q1 with
      | Head (h, subs) -> useful_ctor rows ~siblings ~qrest ~head:h ~fields:subs
      | Tuple ps -> useful_tuple rows ~siblings ~qrest ~fields:ps
      | Wild | Var _ | Record _ -> begin
          match tuple_arity (column rows) with
          | Some n -> useful_tuple rows ~siblings ~qrest ~fields:(wildcards n)
          | None ->
              let sigs = signatures (column rows) in
              if sigs <> [] && complete siblings sigs then
                List.find_map
                  (fun (h, subs) ->
                    useful_ctor rows ~siblings ~qrest ~head:h
                      ~fields:(wildcards (List.length subs)))
                  sigs
              else
                let missing = Option.value (missing_witness siblings sigs) ~default:wildcard in
                useful siblings (default_pats rows) qrest
                |> Option.map (fun w -> missing :: w)
        end
    end

and useful_ctor rows ~siblings ~qrest ~head ~fields =
  let arity = List.length fields in
  useful siblings (specialize_ctor ~head ~arity rows) (fields @ qrest)
  |> Option.map (fun witness ->
         let field_witnesses, rest = split_at arity witness in
         head_pattern head field_witnesses :: rest)

and useful_tuple rows ~siblings ~qrest ~fields =
  let arity = List.length fields in
  useful siblings (specialize_tuple ~arity rows) (fields @ qrest)
  |> Option.map (fun witness ->
         let field_witnesses, rest = split_at arity witness in
         tuple_pattern field_witnesses :: rest)

let counterexample siblings_env patterns =
  let siblings name = Data.Name.Map.find_opt name siblings_env in
  useful siblings (List.map (fun p -> [ p ]) patterns) [ wildcard ]
  |> Option.map (function w :: _ -> w | [] -> wildcard)

let is_exhaustive siblings_env patterns = counterexample siblings_env patterns = None

let redundant_clauses siblings_env patterns =
  let siblings name = Data.Name.Map.find_opt name siblings_env in
  let rec go index above = function
    | [] -> []
    | p :: rest ->
        let below = go (index + 1) ([ p ] :: above) rest in
        if useful siblings above [ p ] = None then index :: below else below
  in
  go 0 [] patterns

type row = {
  pats : P.t list;
  binds : (string * Occurrence.t) list;
  action : int;
}

type matrix = { occs : Occurrence.t list; rows : row list }

let test_of h subs =
  match h with
  | H_ctor n -> if subs = [] then Test_ctor n else Test_tag n
  | H_int i -> Test_int i
  | H_str s -> Test_str s
  | H_chr c -> Test_chr c
  | H_nil -> Test_nil
  | H_cons -> Test_cons

let front_column m = column (List.map (fun row -> row.pats) m.rows)

let bind_aliases front occ0 binds =
  List.fold_left (fun acc name -> (name, occ0) :: acc) binds (alias_names front)

let bind_front front occ0 binds =
  let binds = bind_aliases front occ0 binds in
  match shape_of front with
  | Var x -> (x, occ0) :: binds
  | Record fields ->
      List.fold_left (fun b f -> (f, child occ0 (Field f)) :: b) binds fields
  | Wild | Tuple _ | Head _ -> binds

let swap_list l i =
  match l with
  | [] | [ _ ] -> l
  | x0 :: _ ->
      let xi = List.nth l i in
      List.mapi (fun k x -> if k = 0 then xi else if k = i then x0 else x) l

let swap m i =
  {
    occs = swap_list m.occs i;
    rows = List.map (fun row -> { row with pats = swap_list row.pats i }) m.rows;
  }

let find_product m =
  List.find_map
    (fun i ->
      List.find_map
        (fun row ->
          match shape_of (List.nth row.pats i) with
          | Tuple ps -> Some (i, Some (List.length ps))
          | Record _ -> Some (i, None)
          | Wild | Var _ | Head _ -> None)
        m.rows)
    (List.init (List.length m.occs) Fun.id)

let expand_tuple m n =
  let occ0 = List.hd m.occs and rest_occs = List.tl m.occs in
  let occs = List.init n (fun i -> child occ0 (Index i)) @ rest_occs in
  let rows =
    List.map
      (fun row ->
        match row.pats with
        | front :: rest ->
            let subs, binds =
              match shape_of front with
              | Tuple ps -> (ps, bind_aliases front occ0 row.binds)
              | Wild | Var _ | Record _ | Head _ ->
                  (wildcards n, bind_front front occ0 row.binds)
            in
            { row with pats = subs @ rest; binds }
        | [] -> row)
      m.rows
  in
  { occs; rows }

let expand_record m =
  let occ0 = List.hd m.occs and rest_occs = List.tl m.occs in
  let rows =
    List.map
      (fun row ->
        match row.pats with
        | front :: rest ->
            { row with pats = rest; binds = bind_front front occ0 row.binds }
        | [] -> row)
      m.rows
  in
  { occs = rest_occs; rows }

let best_column siblings m =
  let column_pats i = List.map (fun row -> List.nth row.pats i) m.rows in
  let score_ba i =
    let sigs = signatures (column_pats i) in
    let branching = List.length sigs + if complete siblings sigs then 0 else 1 in
    let arity =
      List.fold_left (fun acc (_, subs) -> acc + List.length subs) 0 sigs
    in
    (-branching, -arity)
  in
  let score_p i =
    let rec prefix j above = function
      | [] -> j
      | row :: rest ->
          let cell = List.nth row.pats i in
          let deleted = List.filteri (fun k _ -> k <> i) row.pats in
          let needed =
            head_of cell <> None || useful siblings above deleted = None
          in
          if needed then prefix (j + 1) (deleted :: above) rest else j
    in
    prefix 0 [] m.rows
  in
  let necessity = List.length m.rows <= 32 in
  let key i = ((if necessity then score_p i else 0), score_ba i) in
  let candidate i = List.exists (fun p -> not (is_wild p)) (column_pats i) in
  match List.filter candidate (List.init (List.length m.occs) Fun.id) with
  | [] -> 0
  | c0 :: cs ->
      List.fold_left (fun best i -> if key i > key best then i else best) c0 cs

let specialization ~head ~arity m =
  let occ0 = List.hd m.occs and rest_occs = List.tl m.occs in
  let suboccs =
    match head with
    | H_ctor _ -> List.init arity (fun i -> child occ0 (Payload i))
    | H_cons -> [ child occ0 Hd; child occ0 Tl ]
    | H_int _ | H_str _ | H_chr _ | H_nil -> []
  in
  let rows =
    List.filter_map
      (fun row ->
        match row.pats with
        | [] -> None
        | front :: rest -> begin
            match head_of front with
            | None ->
                Some
                  {
                    row with
                    pats = wildcards (List.length suboccs) @ rest;
                    binds = bind_front front occ0 row.binds;
                  }
            | Some h ->
                if h = head then
                  Some
                    {
                      row with
                      pats = subpats front @ rest;
                      binds = bind_aliases front occ0 row.binds;
                    }
                else None
          end)
      m.rows
  in
  { occs = suboccs @ rest_occs; rows }

let defaulting m =
  let occ0 = List.hd m.occs and rest_occs = List.tl m.occs in
  let rows =
    List.filter_map
      (fun row ->
        match row.pats with
        | front :: rest when head_of front = None ->
            Some { row with pats = rest; binds = bind_front front occ0 row.binds }
        | _ -> None)
      m.rows
  in
  { occs = rest_occs; rows }

let rec compile siblings m =
  match m.rows with
  | [] -> Fail
  | r0 :: _ when List.for_all is_wild r0.pats ->
      let extra =
        List.concat
          (List.map2
             (fun (p : P.t) occ ->
               List.map (fun name -> (name, occ)) (alias_names p)
               @ begin
                   match shape_of p with
                   | Var x -> [ (x, occ) ]
                   | Wild | Tuple _ | Record _ | Head _ -> []
                 end)
             r0.pats m.occs)
      in
      Leaf { action = r0.action; bindings = r0.binds @ extra }
  | _ :: _ -> begin
      match find_product m with
      | Some (i, tuple_arity) ->
          let m = if i <> 0 then swap m i else m in
          let expanded =
            match tuple_arity with
            | Some n -> expand_tuple m n
            | None -> expand_record m
          in
          compile siblings expanded
      | None ->
          let i = best_column siblings m in
          let m = if i <> 0 then swap m i else m in
          let sigs = signatures (front_column m) in
          let branches =
            List.map
              (fun (h, subs) ->
                ( test_of h subs,
                  compile siblings (specialization ~head:h ~arity:(List.length subs) m) ))
              sigs
          in
          let default =
            if complete siblings sigs then None
            else Some (compile siblings (defaulting m))
          in
          Switch { occurrence = List.hd m.occs; branches; default }
    end

let build siblings patterns =
  compile siblings
    {
      occs = [ root ];
      rows =
        List.mapi (fun action p -> { pats = [ p ]; binds = []; action }) patterns;
    }
