module J = Ast
module DT = After_typed.Exhaustive.Decision_tree
module Occ = After_typed.Exhaustive.Occurrence
module DS = After_typed.Exhaustive.Share

let occ_expr root (o : Occ.t) : J.expr =
  List.fold_left
    (fun e step ->
      match step with
      | Occ.Payload i -> J.member e (Runtime.payload i)
      | Occ.Index i -> J.at_index e i
      | Occ.Field f -> J.member e f
      | Occ.Hd -> J.member e Runtime.head
      | Occ.Tl -> J.member e Runtime.tail)
    root o

let ctor_literal name =
  match Primitives.bool_of_constructor name with
  | Some truth -> J.Bool truth
  | None -> J.String (Data.Name.base name)

let js_eq left literal = J.binary J.StrictEqual left (J.Literal literal)

let test_expr env occ_e (test : DT.test) : J.expr =
  match test with
  | DT.Test_ctor name -> js_eq occ_e (ctor_literal name)
  | DT.Test_tag name ->
      if Names.omits_tag env.Env.names name then J.is_object occ_e
      else js_eq (J.member occ_e Runtime.tag) (J.String (Data.Name.base name))
  | DT.Test_int n -> js_eq occ_e (J.Int n)
  | DT.Test_str s -> js_eq occ_e (J.String s)
  | DT.Test_chr c -> js_eq occ_e (J.String c)
  | DT.Test_nil -> js_eq occ_e (J.Int 0)
  | DT.Test_cons -> J.binary J.StrictNotEqual occ_e (J.int 0)

type discriminant = By_tag | By_value

let same_discriminant one other =
  match (one, other) with
  | By_tag, By_tag | By_value, By_value -> true
  | By_tag, By_value | By_value, By_tag -> false

let switch_key env (test : DT.test) : (discriminant * J.literal) option =
  match test with
  | DT.Test_tag n when not (Names.omits_tag env.Env.names n) ->
      Some (By_tag, J.String (Data.Name.base n))
  | DT.Test_tag _ -> None
  | DT.Test_ctor n when not (Names.is_bool_constructor n) ->
      Some (By_value, J.String (Data.Name.base n))
  | DT.Test_int n -> Some (By_value, J.Int n)
  | DT.Test_str s -> Some (By_value, J.String s)
  | DT.Test_chr c -> Some (By_value, J.String c)
  | DT.Test_ctor _ | DT.Test_nil | DT.Test_cons -> None

let switch_plan env occ_e (branches : (DT.test * DT.t) list) :
    (J.expr * (J.literal * DT.t) list) option =
  let keys =
    List.fold_right
      (fun (test, subtree) collected ->
        match (collected, switch_key env test) with
        | Some cases, Some (kind, literal) ->
            Some ((kind, literal, subtree) :: cases)
        | _ -> None)
      branches (Some [])
  in
  match keys with
  | None | Some [] -> None
  | Some ((kind, _, _) :: _ as cases) ->
      if List.for_all (fun (other, _, _) -> same_discriminant other kind) cases
      then
        let discriminant =
          match kind with By_tag -> J.member occ_e Runtime.tag | By_value -> occ_e
        in
        Some
          ( discriminant,
            List.map (fun (_, literal, subtree) -> (literal, subtree)) cases )
      else None

let match_failure =
  [
    J.Throw
      (J.New
         {
           callee = J.Identifier "Error";
           args = [ J.string "Pattern match failed" ];
         });
  ]

let thunk_names env plan =
  List.map
    (fun (id, _) -> (id, Names.fresh env.Env.names ("$dt" ^ string_of_int id)))
    (DS.nodes plan)

let rec lower env root ~terminating ~leaf ~fail ~sink ~plan ~tnames
    (tree : DT.t) : J.stmt list =
  match DS.id_of plan tree with
  | Some id -> sink (J.call (J.Identifier (List.assoc id tnames)) [])
  | None -> lower_node env root ~terminating ~leaf ~fail ~sink ~plan ~tnames tree

and lower_node env root ~terminating ~leaf ~fail ~sink ~plan ~tnames
    (tree : DT.t) : J.stmt list =
  match tree with
  | DT.Fail -> fail
  | DT.Leaf { action; bindings } ->
      let jbinds =
        List.map (fun (v, o) -> (Data.Name.local v, occ_expr root o)) bindings
      in
      let env', bstmts = Env.bind_binds env jbinds in
      bstmts @ leaf env' action
  | DT.Switch { occurrence; branches; default } -> begin
      let occ_e = occ_expr root occurrence in
      let go tr =
        lower env root ~terminating ~leaf ~fail ~sink ~plan ~tnames tr
      in
      let many_cases (disc, cases) =
        if List.length cases >= 2 then Some (disc, cases) else None
      in
      let switch_of =
        if terminating then switch_plan env occ_e branches else None
      in
      match Option.bind switch_of many_cases with
      | Some (disc, cases) ->
          let default_case =
            default
            |> Option.map (fun t -> { J.test = None; consequent = go t })
            |> Option.to_list
          in
          let js_cases =
            List.map
              (fun (lit, tr) ->
                { J.test = Some (J.Literal lit); consequent = go tr })
              cases
          in
          [ J.Switch { discriminant = disc; cases = js_cases @ default_case } ]
      | None ->
          let rec build = function
            | [] -> Option.fold ~none:fail ~some:go default
            | [ (_, tr) ] when Option.is_none default -> go tr
            | (test, tr) :: rest ->
                [
                  J.If
                    {
                      test = test_expr env occ_e test;
                      consequent = go tr;
                      alternate = Some (build rest);
                    };
                ]
          in
          build branches
    end

let shared_thunks env root ~plan ~tnames clause_expr =
  let sink e = [ J.Return (Some e) ] in
  let leaf env action =
    let sa, ea = clause_expr env action in
    sa @ [ J.Return (Some ea) ]
  in
  List.map
    (fun (id, sub) ->
      let body =
        lower_node env root ~terminating:true ~leaf ~fail:match_failure ~sink
          ~plan ~tnames sub
      in
      J.ConstDecl { name = List.assoc id tnames; init = J.arrow_of_body [] body })
    (DS.nodes plan)
