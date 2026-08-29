module By_module = Map.Make (String)
module Module_names = Set.Make (String)

type error = Import_cycle of string list [@@deriving show]

type traversal = {
  settled : Module_names.t;
  ordered : Canonical.Module.t list;
}

let in_dependency_order (modules : Canonical.Module.t list) :
    (Canonical.Module.t list, error) result =
  let known =
    List.fold_left
      (fun acc (m : Canonical.Module.t) -> By_module.add m.name m acc)
      By_module.empty modules
  in
  let cycle_through ~importing name =
    let rec until_start = function
      | [] -> []
      | current :: outer ->
          if String.equal current name then [ current ]
          else current :: until_start outer
    in
    List.rev (until_start importing)
  in
  let rec visit ~importing traversal name =
    if Module_names.mem name traversal.settled then Ok traversal
    else if List.exists (String.equal name) importing then
      Error (Import_cycle (cycle_through ~importing name))
    else
      match By_module.find_opt name known with
      | None -> Ok traversal
      | Some (m : Canonical.Module.t) ->
          let importing = name :: importing in
          List.fold_left
            (fun traversal (import : Canonical.Import.t) ->
              Result.bind traversal (fun traversal ->
                  visit ~importing traversal import.module_name))
            (Ok traversal) m.imports
          |> Result.map (fun traversal ->
                 {
                   settled = Module_names.add name traversal.settled;
                   ordered = m :: traversal.ordered;
                 })
  in
  List.fold_left
    (fun traversal (m : Canonical.Module.t) ->
      Result.bind traversal (fun traversal -> visit ~importing:[] traversal m.name))
    (Ok { settled = Module_names.empty; ordered = [] })
    modules
  |> Result.map (fun traversal -> List.rev traversal.ordered)
