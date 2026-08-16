module DT = Exhaustive.Decision_tree

type t = { ids : (DT.t, int) Hashtbl.t; order : (int * DT.t) list }

let analyze ~shareable (tree : DT.t) : t =
  let counts : (DT.t, int) Hashtbl.t = Hashtbl.create 32 in
  let rec count (node : DT.t) =
    let seen = match Hashtbl.find_opt counts node with Some n -> n | None -> 0 in
    Hashtbl.replace counts node (seen + 1);
    if seen = 0 then
      match node with
      | DT.Switch { branches; default; _ } ->
          List.iter (fun (_, child) -> count child) branches;
          Option.iter count default
      | DT.Fail | DT.Leaf _ -> ()
  in
  count tree;
  let ids : (DT.t, int) Hashtbl.t = Hashtbl.create 16 in
  let order = ref [] in
  let next = ref 0 in
  let rec visit (node : DT.t) =
    if not (Hashtbl.mem ids node) then begin
      begin
        match node with
        | DT.Switch { branches; default; _ } ->
            List.iter (fun (_, child) -> visit child) branches;
            Option.iter visit default
        | DT.Fail | DT.Leaf _ -> ()
      end;
      match Hashtbl.find_opt counts node with
      | Some n when n >= 2 && shareable node ->
          Hashtbl.add ids node !next;
          order := (!next, node) :: !order;
          incr next
      | _ -> ()
    end
  in
  visit tree;
  { ids; order = List.rev !order }

let id_of (plan : t) (tree : DT.t) : int option = Hashtbl.find_opt plan.ids tree
let shared (plan : t) : (int * DT.t) list = plan.order
