module O = Optimized
module Names = Scope.Names
module By_name = Map.Make (Data.Name)

type 'a component = Acyclic of 'a | Cyclic of 'a list
type error = Bad_recursion of Data.Name.t list

let show_error (Bad_recursion names) =
  Printf.sprintf "these definitions depend on each other in a cycle: %s"
    (String.concat ", " (List.map Data.Name.to_string names))

type vertex = {
  index : int;
  mutable lowlink : int;
  mutable on_stack : bool;
  mutable self_recursive : bool;
}

let strongly_connected_components (vertices : Data.Name.t list)
    ~(successors : Data.Name.t -> Names.t) : Data.Name.t component list =
  let states : (Data.Name.t, vertex) Hashtbl.t = Hashtbl.create 64 in
  let stack : (Data.Name.t * vertex) list ref = ref [] in
  let next_index = ref 0 in
  let components = ref [] in
  let pop_component ~root_index =
    let rec go collected =
      match !stack with
      | (name, state) :: rest when state.index >= root_index ->
          stack := rest;
          state.on_stack <- false;
          go ((name, state) :: collected)
      | _ -> collected
    in
    match go [] with
    | [ (name, state) ] when not state.self_recursive -> Acyclic name
    | members -> Cyclic (List.map fst members)
  in
  let rec visit name =
    let state =
      {
        index = !next_index;
        lowlink = !next_index;
        on_stack = true;
        self_recursive = false;
      }
    in
    incr next_index;
    Hashtbl.replace states name state;
    stack := (name, state) :: !stack;
    let follow successor =
      if Data.Name.equal successor name then state.self_recursive <- true;
      match Hashtbl.find_opt states successor with
      | None ->
          let reached = visit successor in
          state.lowlink <- Int.min state.lowlink reached.lowlink
      | Some reached when reached.on_stack ->
          state.lowlink <- Int.min state.lowlink reached.index
      | Some _ -> ()
    in
    Names.iter follow (successors name);
    if state.lowlink = state.index then
      components := pop_component ~root_index:state.index :: !components;
    state
  in
  List.iter
    (fun name ->
      if not (Hashtbl.mem states name) then ignore (visit name : vertex))
    vertices;
  List.rev !components

let sort_declarations (decls : O.Declaration.t list) :
    (O.Declaration.t list, error) result =
  let name_of (d : O.Declaration.t) =
    Data.Name.local (Data.Located.unwrap d.name)
  in
  let names = List.map name_of decls in
  let declared = Names.of_list names in
  let by_name =
    List.fold_left (fun acc d -> By_name.add (name_of d) d acc) By_name.empty
      decls
  in
  let declaration name = By_name.find_opt name by_name in
  let successors name =
    match declaration name with
    | None -> Names.empty
    | Some d -> Names.inter (Scope.free_in_declaration d) declared
  in
  let evaluated_before_use (d : O.Declaration.t) =
    match d.params with
    | _ :: _ -> false
    | [] -> begin
        match d.body.expr with O.Expr.Expr_lambda _ -> false | _ -> true
      end
  in
  let rec go ordered = function
    | [] -> Ok (List.rev ordered)
    | Acyclic name :: rest ->
        go (List.rev_append (Option.to_list (declaration name)) ordered) rest
    | Cyclic names :: rest ->
        let group = List.filter_map declaration names in
        if List.exists evaluated_before_use group then Error (Bad_recursion names)
        else go (List.rev_append group ordered) rest
  in
  go [] (strongly_connected_components names ~successors)
