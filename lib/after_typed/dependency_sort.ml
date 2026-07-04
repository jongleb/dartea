open Base
module O = Optimized

let rec free_vars_expr (bound : Set.M(String).t) (expr : O.Expr.t) :
    Set.M(String).t =
  match expr.expr with
  | Expr_ident name ->
      if Set.mem bound name then Set.empty (module String)
      else Set.singleton (module String) name
  | Expr_let let_expr ->
      let bind_fv = free_vars_expr bound let_expr.binding.bind_body.body in
      let bound' =
        Set.add bound (Data.Located.unwrap let_expr.binding.bind_body.name)
      in
      let body_fv = free_vars_expr bound' let_expr.body in
      Set.union bind_fv body_fv
  | Expr_lambda lambda ->
      let bound' =
        List.fold lambda.params ~init:bound
          ~f:(fun acc (p : O.Expr.expr_lambda_param) ->
            Set.add acc (Data.Located.unwrap p.name))
      in
      free_vars_expr bound' lambda.body
  | Expr_apply apply ->
      Set.union (free_vars_expr bound apply.fn) (free_vars_expr bound apply.arg)
  | Expr_binop binop ->
      let left, right = binop.operands in
      let fv =
        Set.union (free_vars_expr bound left) (free_vars_expr bound right)
      in
      Set.add fv binop.name
  | Expr_if_then_else ite ->
      Set.union (free_vars_expr bound ite.if_exp)
        (Set.union
           (free_vars_expr bound ite.then_exp)
           (free_vars_expr bound ite.else_exp))
  | Expr_constr constr ->
      List.fold constr.arguments ~init:(Set.empty (module String))
        ~f:(fun acc arg -> Set.union acc (free_vars_expr bound arg))
  | Expr_list exprs ->
      List.fold exprs ~init:(Set.empty (module String)) ~f:(fun acc e ->
        Set.union acc (free_vars_expr bound e))
  | Expr_record rows ->
      List.fold rows ~init:(Set.empty (module String))
        ~f:(fun acc (row : O.Expr.expr_record_row) ->
          Set.union acc (free_vars_expr bound row.value))
  | Expr_pattern pat ->
      let scrutinee_fv = free_vars_expr bound pat.expr in
      let cases_fv =
        List.fold pat.pattern_data_items ~init:(Set.empty (module String))
          ~f:(fun acc (case : O.Expr.expr_pattern_case) ->
            Set.union acc (free_vars_expr bound case.expr))
      in
      Set.union scrutinee_fv cases_fv
  | Expr_access access -> free_vars_expr bound access.expr
  | Expr_accessor _ | Expr_record_extend _ | Expr_record_select _
  | Expr_record_empty | Expr_char _ | Expr_string _ | Expr_int _
  | Expr_float _ ->
      Set.empty (module String)

let free_vars (decl : O.Declaration.t) : Set.M(String).t =
  let bound =
    List.fold decl.params ~init:(Set.empty (module String))
      ~f:(fun acc (p : O.Declaration.param) ->
        Set.add acc (Data.Located.unwrap p.name))
  in
  free_vars_expr bound decl.body

(* Tarjan's SCC algorithm *)
let tarjan_scc (nodes : string list)
    (adj : (string, Set.M(String).t) List.Assoc.t) : string list list =
  let index_counter = ref 0 in
  let stack = ref [] in
  let on_stack = Hashtbl.create (module String) in
  let indices = Hashtbl.create (module String) in
  let lowlinks = Hashtbl.create (module String) in
  let result = ref [] in
  let rec strongconnect v =
    let idx = !index_counter in
    Int.incr index_counter;
    Hashtbl.set indices ~key:v ~data:idx;
    Hashtbl.set lowlinks ~key:v ~data:idx;
    stack := v :: !stack;
    Hashtbl.set on_stack ~key:v ~data:true;
    let successors =
      match List.Assoc.find adj v ~equal:String.equal with
      | Some s -> s
      | None -> Set.empty (module String)
    in
    Set.iter successors ~f:(fun w ->
      if not (Hashtbl.mem indices w) then begin
        strongconnect w;
        Hashtbl.set lowlinks ~key:v
          ~data:(Int.min (Hashtbl.find_exn lowlinks v) (Hashtbl.find_exn lowlinks w))
      end
      else if Option.value (Hashtbl.find on_stack w) ~default:false then
        Hashtbl.set lowlinks ~key:v
          ~data:(Int.min (Hashtbl.find_exn lowlinks v) (Hashtbl.find_exn indices w)));
    if Hashtbl.find_exn lowlinks v = Hashtbl.find_exn indices v then begin
      let component = ref [] in
      let continue = ref true in
      while !continue do
        match !stack with
        | w :: rest ->
            stack := rest;
            Hashtbl.set on_stack ~key:w ~data:false;
            component := w :: !component;
            if String.equal w v then continue := false
        | [] -> continue := false
      done;
      result := !component :: !result
    end
  in
  List.iter nodes ~f:(fun v ->
    if not (Hashtbl.mem indices v) then strongconnect v);
  List.rev !result

let sort_declarations (decls : O.Declaration.t list) : O.Declaration.t list =
  let decl_names =
    List.fold decls ~init:(Set.empty (module String))
      ~f:(fun acc (d : O.Declaration.t) ->
        Set.add acc (Data.Located.unwrap d.name))
  in
  let decl_map =
    List.map decls ~f:(fun (d : O.Declaration.t) ->
      (Data.Located.unwrap d.name, d))
  in
  (* Build adjacency: name -> set of names it depends on (within this module) *)
  let adj =
    List.map decls ~f:(fun (d : O.Declaration.t) ->
      let name = Data.Located.unwrap d.name in
      let fv = free_vars d in
      (name, Set.inter fv decl_names))
  in
  let nodes =
    List.map decls ~f:(fun (d : O.Declaration.t) -> Data.Located.unwrap d.name)
  in
  (* Find SCCs — each component is a group of mutually recursive decls *)
  let sccs = tarjan_scc nodes adj in
  (* SCCs are already in reverse topological order from Tarjan's,
     but we built result with List.rev so they're in topological order *)
  (* Topologically sort the SCCs: a component must come after all
     components it depends on. Tarjan gives reverse topo order of the
     condensation, and we reversed at the end, so sccs is in topo order. *)
  List.concat_map sccs ~f:(fun component ->
    List.filter_map component ~f:(fun name ->
      List.Assoc.find decl_map ~equal:String.equal name))
