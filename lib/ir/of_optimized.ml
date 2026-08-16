module O = Optimized
module DT = After_typed.Exhaustive.Decision_tree
module Occ = After_typed.Exhaustive.Occurrence
module Scope = After_typed.Scope
module By_name = Map.Make (Data.Name)
module Text = Set.Make (String)

module By_occurrence = Map.Make (struct
  type t = Occ.step list

  let compare = compare
end)

open Ast

type continuation =
  | K_return
  | K_jump of string
  | K_let of { name : string; body : term }

type case = {
  locals : Text.t;
  inner : continuation;
  root : atom;
  arms : O.Expr.expr_pattern_case list;
}

type context = {
  constructor_arity : int By_name.t;
  toplevel_arity : int By_name.t;
  siblings_of : Data.Name.t -> (Data.Name.t * int) list option;
  operators : Text.t;
}

let spine expression =
  let rec collect arguments (e : O.Expr.t) =
    match e.expr with
    | O.Expr.Expr_apply { fn; arg } -> collect (arg :: arguments) fn
    | _ -> (e, arguments)
  in
  collect [] expression

let record_spine expression =
  let rec walk fields (e : O.Expr.t) =
    let stop () =
      match fields with [] -> None | _ -> Some (List.rev fields, Some e)
    in
    match e.expr with
    | O.Expr.Expr_record_empty -> Some (List.rev fields, None)
    | O.Expr.Expr_apply _ -> begin
        match spine e with
        | { O.Expr.expr = O.Expr.Expr_record_extend field; _ }, [ value; rest ]
          ->
            walk ((field, value) :: fields) rest
        | _ -> stop ()
      end
    | _ -> stop ()
  in
  walk [] expression

let split_last steps =
  let rec go seen = function
    | [] -> None
    | [ last ] -> Some (List.rev seen, last)
    | step :: rest -> go (step :: seen) rest
  in
  go [] steps

let split_at count items =
  let rec go taken count items =
    match (count, items) with
    | 0, rest -> (List.rev taken, rest)
    | _, [] -> (List.rev taken, [])
    | _, item :: rest -> go (item :: taken) (count - 1) rest
  in
  go [] count items

let local_names names =
  Scope.Names.fold
    (fun name found ->
      match name with
      | Data.Name.Local text -> text :: found
      | Data.Name.Global _ -> found)
    names []

let wanted_by_constructor context name =
  Option.value ~default:0 (By_name.find_opt name context.constructor_arity)

let refers_locally ~locals (name : Data.Name.t) =
  match name with
  | Data.Name.Local text -> Text.mem text locals
  | Data.Name.Global _ -> false

let is_primitive_operator context ~locals (name : Data.Name.t) =
  match name with
  | Data.Name.Local text ->
      (not (Text.mem text locals))
      && (not (By_name.mem name context.toplevel_arity))
      && Text.mem text context.operators
  | Data.Name.Global _ -> false

let name_atom ~locals (name : Data.Name.t) =
  match name with
  | Data.Name.Local text when Text.mem text locals -> A_var text
  | Data.Name.Local _ | Data.Name.Global _ -> A_global name

let immediate context ~locals (e : O.Expr.t) =
  match e.expr with
  | O.Expr.Expr_int value -> Some (A_int value)
  | O.Expr.Expr_float value -> Some (A_float value)
  | O.Expr.Expr_string value -> Some (A_string value)
  | O.Expr.Expr_char value -> Some (A_char value)
  | O.Expr.Expr_unit -> Some A_unit
  | O.Expr.Expr_list [] -> Some A_nil
  | O.Expr.Expr_ident name -> Some (name_atom ~locals name)
  | O.Expr.Expr_constr { name; arguments = [] }
    when wanted_by_constructor context name = 0 ->
      Some (A_constant name)
  | _ -> None

let of_declaration context (source : O.Declaration.t) : declaration =
  let counter = ref 0 in
  let fresh prefix =
    incr counter;
    "#" ^ prefix ^ string_of_int !counter
  in
  let apply k atom =
    match k with
    | K_return -> T_return atom
    | K_jump label -> T_jump { label; arguments = [ atom ] }
    | K_let { name; body } -> T_let { name; bind = B_atom atom; body }
  in
  let emit k bind =
    match k with
    | K_let { name; body } -> T_let { name; bind; body }
    | K_return ->
        let name = fresh "v" in
        T_let { name; bind; body = T_return (A_var name) }
    | K_jump label ->
        let name = fresh "v" in
        T_let
          { name; bind; body = T_jump { label; arguments = [ A_var name ] } }
  in
  let with_join k build =
    match k with
    | K_return | K_jump _ -> build k
    | K_let { name; body } ->
        let label = fresh "join" in
        T_join
          {
            label;
            parameters = [ name ];
            definition = body;
            body = build (K_jump label);
          }
  in
  let rec normalize ~locals ~k (e : O.Expr.t) : term =
    match e.expr with
    | O.Expr.Expr_int value -> apply k (A_int value)
    | O.Expr.Expr_float value -> apply k (A_float value)
    | O.Expr.Expr_string value -> apply k (A_string value)
    | O.Expr.Expr_char value -> apply k (A_char value)
    | O.Expr.Expr_unit -> apply k A_unit
    | O.Expr.Expr_ident name -> apply k (name_atom ~locals name)
    | O.Expr.Expr_record_empty -> emit k (B_record { fields = [] })
    | O.Expr.Expr_accessor field ->
        emit k (selector_closure (Data.Located.unwrap field))
    | O.Expr.Expr_record_select field -> emit k (selector_closure field)
    | O.Expr.Expr_record_extend field -> emit k (extension_closure field)
    | O.Expr.Expr_let { binding = { bind_body = { name; body = bound } }; body }
      ->
        let text = Data.Located.unwrap name in
        let rest = normalize ~locals:(Text.add text locals) ~k body in
        normalize ~locals bound ~k:(K_let { name = text; body = rest })
    | O.Expr.Expr_if_then_else { if_exp; then_exp; else_exp } ->
        normalize_atom ~locals if_exp ~k:(fun condition ->
            with_join k (fun inner ->
                T_if
                  {
                    condition;
                    consequent = normalize ~locals ~k:inner then_exp;
                    alternative = normalize ~locals ~k:inner else_exp;
                  }))
    | O.Expr.Expr_lambda { params; body } ->
        emit k (closure ~locals ~params ~body)
    | O.Expr.Expr_list items -> lower_list ~locals ~k items
    | O.Expr.Expr_record rows -> lower_record_rows ~locals ~k rows
    | O.Expr.Expr_access { expr; field } ->
        normalize_atom ~locals expr ~k:(fun subject ->
            emit k
              (B_access
                 { subject; step = Occ.Field (Data.Located.unwrap field) }))
    | O.Expr.Expr_constr { name; arguments } ->
        lower_construct ~locals ~k ~name arguments
    | O.Expr.Expr_binop { name; operands = left, right } ->
        normalize_atoms ~locals [ left; right ] ~k:(fun atoms ->
            emit k (B_primitive { operator = name; arguments = atoms }))
    | O.Expr.Expr_kernel kernel -> lower_kernel ~locals ~k kernel
    | O.Expr.Expr_pattern { expr; pattern_data_items } ->
        lower_case ~locals ~k ~arms:pattern_data_items expr
    | O.Expr.Expr_apply _ -> begin
        match record_spine e with
        | Some (fields, base) -> lower_record ~locals ~k ~base fields
        | None -> lower_apply ~locals ~k e
      end

  and normalize_atom ~locals e ~(k : atom -> term) =
    match immediate context ~locals e with
    | Some atom -> k atom
    | None ->
        let name = fresh "a" in
        normalize ~locals e ~k:(K_let { name; body = k (A_var name) })

  and normalize_atoms ~locals items ~(k : atom list -> term) =
    let rec go collected = function
      | [] -> k (List.rev collected)
      | item :: rest ->
          normalize_atom ~locals item ~k:(fun atom ->
              go (atom :: collected) rest)
    in
    go [] items

  and closure ~locals ~params ~body =
    let parameters =
      List.map
        (fun (param : O.Expr.expr_lambda_param) ->
          Data.Located.unwrap param.name)
        params
    in
    let captures =
      Scope.free_variables ~bound:(Scope.bound_by_lambda params) body
      |> local_names
      |> List.filter (fun text -> Text.mem text locals)
      |> List.sort_uniq String.compare
    in
    let inner =
      List.fold_left
        (fun known text -> Text.add text known)
        (Text.of_list captures) parameters
    in
    B_closure
      { parameters; captures; body = normalize ~locals:inner ~k:K_return body }

  and missing_arguments_closure ~missing ~build given =
    let parameters = List.init missing (fun _ -> fresh "p") in
    let captures =
      List.filter_map (function A_var text -> Some text | _ -> None) given
      |> List.sort_uniq String.compare
    in
    let name = fresh "v" in
    B_closure
      {
        parameters;
        captures;
        body =
          T_let
            {
              name;
              bind =
                build (given @ List.map (fun text -> A_var text) parameters);
              body = T_return (A_var name);
            };
      }

  and selector_closure field =
    let record = fresh "r" in
    let value = fresh "v" in
    B_closure
      {
        parameters = [ record ];
        captures = [];
        body =
          T_let
            {
              name = value;
              bind = B_access { subject = A_var record; step = Occ.Field field };
              body = T_return (A_var value);
            };
      }

  and extension_closure field =
    let value = fresh "v" in
    let record = fresh "r" in
    let extended = fresh "e" in
    B_closure
      {
        parameters = [ value; record ];
        captures = [];
        body =
          T_let
            {
              name = extended;
              bind =
                B_record_update
                  { base = A_var record; fields = [ (field, A_var value) ] };
              body = T_return (A_var extended);
            };
      }

  and lower_list ~locals ~k items =
    normalize_atoms ~locals items ~k:(fun atoms ->
        let rec chain tail = function
          | [] -> apply k tail
          | [ head ] -> emit k (B_cons { head; tail })
          | head :: rest ->
              let name = fresh "l" in
              T_let
                {
                  name;
                  bind = B_cons { head; tail };
                  body = chain (A_var name) rest;
                }
        in
        chain A_nil (List.rev atoms))

  and lower_record_rows ~locals ~k rows =
    normalize_atoms ~locals
      (List.map (fun (row : O.Expr.expr_record_row) -> row.value) rows)
      ~k:(fun atoms ->
        let named =
          List.map2
            (fun (row : O.Expr.expr_record_row) atom -> (row.name, atom))
            rows atoms
        in
        emit k (B_record { fields = named }))

  and lower_record ~locals ~k ~base fields =
    normalize_atoms ~locals (List.map snd fields) ~k:(fun atoms ->
        let named =
          List.map2 (fun (label, _) atom -> (label, atom)) fields atoms
        in
        match base with
        | None -> emit k (B_record { fields = named })
        | Some expression ->
            normalize_atom ~locals expression ~k:(fun subject ->
                emit k (B_record_update { base = subject; fields = named })))

  and lower_construct ~locals ~k ~name arguments =
    normalize_atoms ~locals arguments ~k:(fun atoms ->
        let wanted = wanted_by_constructor context name in
        let given = List.length atoms in
        if given >= wanted then emit k (B_construct { name; arguments = atoms })
        else
          emit k
            (missing_arguments_closure ~missing:(wanted - given)
               ~build:(fun all -> B_construct { name; arguments = all })
               atoms))

  and lower_kernel ~locals ~k (kernel : O.Expr.expr_kernel) =
    match kernel with
    | O.Expr.Kernel_unary { kernel; argument } ->
        normalize_atom ~locals argument ~k:(fun atom ->
            emit k
              (B_kernel
                 { kernel = Data.Kernel.Unary kernel; arguments = [ atom ] }))
    | O.Expr.Kernel_binary { kernel; left; right } ->
        normalize_atoms ~locals [ left; right ] ~k:(fun atoms ->
            emit k
              (B_kernel
                 { kernel = Data.Kernel.Binary kernel; arguments = atoms }))
    | O.Expr.Kernel_value kernel ->
        emit k
          (missing_arguments_closure ~missing:(Data.Kernel.arity kernel)
             ~build:(fun all -> B_kernel { kernel; arguments = all })
             [])

  and lower_apply ~locals ~k expression =
    let head, arguments = spine expression in
    match head.O.Expr.expr with
    | O.Expr.Expr_ident name when is_primitive_operator context ~locals name ->
        lower_operator ~locals ~k ~operator:(Data.Name.base name) arguments
    | O.Expr.Expr_ident name when refers_locally ~locals name ->
        normalize_atoms ~locals arguments ~k:(fun atoms ->
            emit k
              (B_call_closure
                 { callee = A_var (Data.Name.base name); arguments = atoms }))
    | O.Expr.Expr_ident name ->
        normalize_atoms ~locals arguments ~k:(fun atoms ->
            match By_name.find_opt name context.toplevel_arity with
            | Some arity -> saturated ~k ~callee:name ~arity atoms
            | None ->
                emit k
                  (B_call_closure { callee = A_global name; arguments = atoms }))
    | _ ->
        normalize_atom ~locals head ~k:(fun callee ->
            normalize_atoms ~locals arguments ~k:(fun atoms ->
                emit k (B_call_closure { callee; arguments = atoms })))

  and lower_operator ~locals ~k ~operator arguments =
    normalize_atoms ~locals arguments ~k:(fun atoms ->
        let given = List.length atoms in
        if given >= 2 then emit k (B_primitive { operator; arguments = atoms })
        else
          emit k
            (missing_arguments_closure ~missing:(2 - given)
               ~build:(fun all -> B_primitive { operator; arguments = all })
               atoms))

  and saturated ~k ~callee ~arity atoms =
    let given = List.length atoms in
    if arity = 0 then
      emit k (B_call_closure { callee = A_global callee; arguments = atoms })
    else if given = arity then emit k (B_call { callee; arguments = atoms })
    else if given < arity then
      emit k (B_partial { callee; arguments = atoms; missing = arity - given })
    else
      let direct, rest = split_at arity atoms in
      let name = fresh "v" in
      T_let
        {
          name;
          bind = B_call { callee; arguments = direct };
          body =
            emit k (B_call_closure { callee = A_var name; arguments = rest });
        }

  and lower_case ~locals ~k ~arms subject =
    normalize_atom ~locals subject ~k:(fun root ->
        with_join k (fun inner ->
            let tree =
              After_typed.Exhaustive.build context.siblings_of
                (List.map
                   (fun (arm : O.Expr.expr_pattern_case) -> arm.pattern)
                   arms)
            in
            lower_tree { locals; inner; root; arms } ~known:By_occurrence.empty
              tree))

  and access ~root ~known occurrence ~k =
    match By_occurrence.find_opt occurrence known with
    | Some name -> k (A_var name) known
    | None -> begin
        match split_last occurrence with
        | None -> k root known
        | Some (parent, step) ->
            access ~root ~known parent ~k:(fun subject known ->
                let name = fresh "f" in
                T_let
                  {
                    name;
                    bind = B_access { subject; step };
                    body =
                      k (A_var name) (By_occurrence.add occurrence name known);
                  })
      end

  and lower_leaf case ~known ~bindings action =
    match List.nth_opt case.arms action with
    | None -> T_fail { message = "unreachable pattern branch" }
    | Some (arm : O.Expr.expr_pattern_case) ->
        let bound = Scope.bound_by_pattern arm.pattern |> local_names in
        let inside =
          List.fold_left
            (fun known text -> Text.add text known)
            case.locals bound
        in
        let rec bind ~known = function
          | [] -> normalize ~locals:inside ~k:case.inner arm.expr
          | text :: rest -> begin
              match List.assoc_opt text bindings with
              | None -> bind ~known rest
              | Some occurrence ->
                  access ~root:case.root ~known occurrence ~k:(fun atom known ->
                      T_let
                        {
                          name = text;
                          bind = B_atom atom;
                          body = bind ~known rest;
                        })
            end
        in
        bind ~known bound

  and lower_tree case ~known (node : DT.t) =
    match node with
    | DT.Fail -> T_fail { message = "non-exhaustive pattern match" }
    | DT.Leaf { action; bindings } -> lower_leaf case ~known ~bindings action
    | DT.Switch { occurrence; branches; default } ->
        access ~root:case.root ~known occurrence ~k:(fun subject known ->
            T_switch
              {
                subject;
                branches =
                  List.map
                    (fun (test, child) -> (test, lower_tree case ~known child))
                    branches;
                default = Option.map (lower_tree case ~known) default;
              })
  in
  let parameters =
    List.map
      (fun (param : O.Declaration.param) -> Data.Located.unwrap param.name)
      source.params
  in
  {
    name = Data.Located.unwrap source.name;
    parameters;
    body = normalize ~locals:(Text.of_list parameters) ~k:K_return source.body;
  }

let convert ~arities ~constructors ~siblings
    (declarations : O.Declaration.t list) =
  let constructor_arity =
    List.fold_left
      (fun table (name, arity) -> By_name.add name arity table)
      By_name.empty constructors
  in
  let toplevel_arity =
    List.fold_left
      (fun table (name, arity) -> By_name.add name arity table)
      (List.fold_left
         (fun table (declaration : O.Declaration.t) ->
           By_name.add
             (Data.Name.local (Data.Located.unwrap declaration.name))
             (List.length declaration.params)
             table)
         By_name.empty declarations)
      arities
  in
  let sibling_table =
    List.fold_left
      (fun table (name, family) -> By_name.add name family table)
      By_name.empty siblings
  in
  let context =
    {
      constructor_arity;
      toplevel_arity;
      siblings_of = (fun name -> By_name.find_opt name sibling_table);
      operators =
        List.fold_left
          (fun known (operator, _) -> Text.add operator known)
          Text.empty Primitives.values;
    }
  in
  List.map (of_declaration context) declarations
