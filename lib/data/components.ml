type 'a component = Acyclic of 'a | Cyclic of 'a list

type vertex = {
  index : int;
  mutable lowlink : int;
  mutable on_stack : bool;
  mutable self_recursive : bool;
}

let of_graph ~(name : 'a -> Name.t) ~(depends_on : 'a -> Name.Set.t)
    (members : 'a list) : 'a component list =
  let known =
    List.fold_left
      (fun known member -> Name.Map.add (name member) member known)
      Name.Map.empty members
  in
  let neighbours member =
    Name.Set.fold
      (fun dependency next ->
        match Name.Map.find_opt dependency known with
        | None -> next
        | Some neighbour -> neighbour :: next)
      (depends_on member) []
    |> List.rev
  in
  let states : (Name.t, vertex) Hashtbl.t = Hashtbl.create 64 in
  let stack : ('a * vertex) list ref = ref [] in
  let next_index = ref 0 in
  let components = ref [] in
  let pop_component ~root_index =
    let rec go collected =
      match !stack with
      | (member, state) :: rest when state.index >= root_index ->
          stack := rest;
          state.on_stack <- false;
          go ((member, state) :: collected)
      | _ -> collected
    in
    match go [] with
    | [ (member, state) ] when not state.self_recursive -> Acyclic member
    | grouped -> Cyclic (List.map fst grouped)
  in
  let rec visit member =
    let state =
      {
        index = !next_index;
        lowlink = !next_index;
        on_stack = true;
        self_recursive = false;
      }
    in
    incr next_index;
    Hashtbl.replace states (name member) state;
    stack := (member, state) :: !stack;
    let follow neighbour =
      if Name.equal (name neighbour) (name member) then
        state.self_recursive <- true;
      match Hashtbl.find_opt states (name neighbour) with
      | None ->
          let next = visit neighbour in
          state.lowlink <- Int.min state.lowlink next.lowlink
      | Some next when next.on_stack ->
          state.lowlink <- Int.min state.lowlink next.index
      | Some _ -> ()
    in
    List.iter follow (neighbours member);
    if state.lowlink = state.index then
      components := pop_component ~root_index:state.index :: !components;
    state
  in
  List.iter
    (fun member ->
      if not (Hashtbl.mem states (name member)) then
        ignore (visit member : vertex))
    members;
  List.rev !components

let members = function Acyclic member -> [ member ] | Cyclic grouped -> grouped
