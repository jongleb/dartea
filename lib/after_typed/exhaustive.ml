open CCOption
open Infer
open Typed.Pattern

module Matrix = struct
  open Infer

  type t = Typed.Pattern.t List.t List.t

  let rec detuple full pat =
    match full with
    | { pattern = P_T_tuple list } -> (
        match pat with
        | { pattern = P_T_anything } | { pattern = P_T_var _ } ->
            List.concat
            @@ List.map
                 (fun full2 ->
                   detuple full2 { pattern = P_T_anything; typ = full2.typ })
                 list
        | { pattern = P_T_tuple list2 } ->
            List.concat @@ List.map2 detuple list list2
        | _ -> [ pat ])
    | _ -> [ pat ]

  let of_typed_list list =
    List.mapi (fun idx row -> detuple (List.hd list) row) list

  let is_irrefutable = function
    | { pattern = P_T_anything; _ } | { pattern = P_T_var _; _ } -> true
    | _ -> false

  let get_column lst col_index =
    List.map
      (fun row ->
        if List.length row <= col_index then failwith "Invalid column index"
        else List.nth row col_index)
      lst
end

module Actions = struct
  type t = int List.t

  let of_records r = r |> List.mapi (fun i (_pat : Typed.Pattern.t) -> i)
end

module Compile_state = struct
  type t = { patterns : Matrix.t; actions : Actions.t }

  let make rows =
    let actions = Actions.of_records rows in
    let patterns = Matrix.of_typed_list rows in
    { actions; patterns }
end

module Decision_tree = struct
  open Base.Export

  type t =
    | Fail
    | Leaf of { redudants : int list }
    | Switch of { default : t option; specialized : t list }
  [@@deriving show, compare, equal, hash]
end

open Compile_state
open Decision_tree

let is_equal_signature a b =
  match (a, b) with
  | { pattern = P_T_ctor (a, _) }, { pattern = P_T_ctor (b, _) } -> a = b
  | { pattern = P_T_int a }, { pattern = P_T_int b } -> a = b
  | { pattern = P_T_anything }, { pattern = P_T_anything }
  | { pattern = P_T_var _ }, { pattern = P_T_var _ } ->
      true
  | _ -> false
(* | _ ->
    failwith
      (Printf.sprintf "Currently unsupported %s, %s" (Typed.show a)
         (Typed.show b)) *)

let uniq_signatures ps =
  ps
  |> List.filter (Fun.negate @@ Matrix.is_irrefutable)
  |> CCList.uniq ~eq:is_equal_signature

let specialization state pat =
  let patterns, actions =
    CCList.fold_left2
      (fun acc row action ->
        let remaining_rows, remaining_actions = acc in
        let pat' = List.hd row in
        if is_equal_signature pat' pat || Matrix.is_irrefutable pat' then
          let row =
            match (pat, pat') with
            | ( { pattern = P_T_ctor (_, list); _ },
                { pattern = P_T_ctor (_, list2); _ } ) ->
                List.concat
                @@ List.map2 (fun a b -> Matrix.detuple a b) list list2
            | { pattern = P_T_ctor (_, list); typ = TCustom (_, typs) }, p ->
                Matrix.detuple { pattern = P_T_tuple list; typ = TTup typs } p
            | _, p ->
                row |> CCList.tail_opt |> CCOption.to_list |> CCList.concat
          in
          let row =
            if row = [] then [ { pattern = P_T_anything; typ = TUnit } ]
            else row
          in
          (row :: remaining_rows, action :: remaining_actions)
        else acc)
      ([], []) state.patterns state.actions
  in
  { patterns = List.rev patterns; actions = List.rev actions }

let defaulting state =
  let patterns, actions =
    CCList.fold_left2
      (fun acc row action ->
        let remaining_rows, remaining_actions = acc in
        let pat = List.hd row in
        if Matrix.is_irrefutable pat then
          (row :: remaining_rows, action :: remaining_actions)
        else acc)
      ([], []) state.patterns state.actions
  in

  { patterns = patterns |> List.rev; actions = actions |> List.rev }

let swap node i j =
  {
    node with
    patterns =
      List.map
        (fun row ->
          if List.length row <= max i j then failwith "Invalid indices"
          else
            let elem_i = List.nth row i in
            let elem_j = List.nth row j in
            List.mapi
              (fun k x -> if k = i then elem_j else if k = j then elem_i else x)
              row)
        node.patterns;
  }

let first_refutable node =
  node.patterns |> List.hd
  |> CCList.find_mapi (fun idx _ ->
         if
           List.exists
             (fun row ->
               CCList.get_at_idx idx row
               |> CCOption.get_exn_or "not found"
               |> Matrix.is_irrefutable |> not)
             node.patterns
         then Some idx
         else None)
  |> CCOption.get_exn_or "InvalidOperationException"

let arities = function
  | { pattern = P_T_ctor (name, _) } -> (
      (*TODO FIX ME*)
      match name with
      | "None" | "Just" -> 2
      | "A" | "B" -> 2
      | "C" | "D" -> 2
      | "E" | "F" | "G" -> 3
      | _ -> failwith "no implemented")
  | p when Matrix.is_irrefutable p -> 0
  | _ -> Int.max_int

let rec compile node =
  if List.length node.patterns = 0 then Fail
  else if List.for_all Matrix.is_irrefutable (List.hd node.patterns) then
    let _result = List.hd node.actions in
    let redudants =
      let length = List.length node.patterns in
      List.init (length - 1) (( + ) 1)
    in
    Leaf { redudants }
  else
    let first_refutable = first_refutable node in
    let node =
      if first_refutable <> 0 then swap node 0 first_refutable else node
    in
    let uniq = uniq_signatures (Matrix.get_column node.patterns 0) in
    let specialized =
      List.map
        (fun signature -> signature |> specialization node |> compile)
        uniq
    in
    let default =
      if List.length uniq < arities (List.hd @@ List.hd node.patterns) then
        Some (compile @@ defaulting node)
      else None
    in
    Switch { specialized; default }

let rec is_exhaustive' = function
  | Fail -> false
  | Leaf _ -> true
  | Switch { default; specialized } ->
      List.for_all is_exhaustive'
        (List.concat [ CCOption.to_list default; specialized ])

let is_exhaustive rows = rows |> Compile_state.make |> compile |> is_exhaustive'
