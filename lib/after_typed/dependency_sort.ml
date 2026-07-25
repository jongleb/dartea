module O = Optimized
module Names = Expr_walk.Names
module By_name = Map.Make (String)

type vertex = { index : int; mutable lowlink : int; mutable on_stack : bool }

let strongly_connected_components (vertices : string list)
    (edges : Names.t By_name.t) : string list list =
  let states : (string, vertex) Hashtbl.t = Hashtbl.create 64 in
  let stack = ref [] in
  let next_index = ref 0 in
  let components = ref [] in
  let rec pop_component ~root collected =
    match !stack with
    | [] -> collected
    | name :: rest ->
        stack := rest;
        Option.iter
          (fun state -> state.on_stack <- false)
          (Hashtbl.find_opt states name);
        if String.equal name root then name :: collected
        else pop_component ~root (name :: collected)
  in
  let rec visit name =
    let state =
      { index = !next_index; lowlink = !next_index; on_stack = true }
    in
    incr next_index;
    Hashtbl.replace states name state;
    stack := name :: !stack;
    let follow successor =
      match Hashtbl.find_opt states successor with
      | None ->
          let reached = visit successor in
          state.lowlink <- Int.min state.lowlink reached.lowlink
      | Some reached ->
          if reached.on_stack then
            state.lowlink <- Int.min state.lowlink reached.index
    in
    Names.iter follow
      (By_name.find_opt name edges |> Option.value ~default:Names.empty);
    if state.lowlink = state.index then
      components := pop_component ~root:name [] :: !components;
    state
  in
  let visit_unvisited name =
    if not (Hashtbl.mem states name) then
      let (_ : vertex) = visit name in
      ()
  in
  List.iter visit_unvisited vertices;
  List.rev !components

let sort_declarations (decls : O.Declaration.t list) : O.Declaration.t list =
  let name_of (d : O.Declaration.t) = Data.Located.unwrap d.name in
  let names = List.map name_of decls in
  let declared = Names.of_list names in
  let by_name =
    List.fold_left (fun acc d -> By_name.add (name_of d) d acc) By_name.empty
      decls
  in
  let edges =
    List.fold_left
      (fun acc d ->
        By_name.add (name_of d)
          (Names.inter (Expr_walk.free_in_declaration d) declared)
          acc)
      By_name.empty decls
  in
  let declaration name = By_name.find_opt name by_name in
  strongly_connected_components names edges
  |> List.concat_map (List.filter_map declaration)
