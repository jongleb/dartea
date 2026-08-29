type view = {
  versions : string -> Version.t list;
  dependencies : string -> Version.t -> (string * Version.Range.t) list;
}

type problem =
  | Unknown_package of { package : string; asked_by : string }
  | No_version of { package : string; asked : (string * string) list }

type need = { bounds : Version.Interval.t; asked : (string * Version.Range.t) list }

type state = {
  needs : (string * need) list;
  chosen : (string * Version.t) list;
}

let root = "your dependencies"

let unsatisfied package need =
  let shown (who, range) = (who, Version.Range.show range) in
  No_version { package; asked = List.map shown need.asked }

let asker need =
  match need.asked with [] -> root | (who, _) :: _ -> who

let swapped ~package need (name, held) =
  if String.equal name package then (name, need) else (name, held)

let replaced ~package need needs =
  match List.assoc_opt package needs with
  | None -> needs @ [ (package, need) ]
  | Some _ -> List.map (swapped ~package need) needs

let settled state package need =
  match List.assoc_opt package state.chosen with
  | Some version when not (Version.Interval.holds need.bounds version) ->
      Error (unsatisfied package need)
  | Some _ | None ->
      Ok { state with needs = replaced ~package need state.needs }

let merged state package held wanting =
  let asked = held.asked @ wanting.asked in
  match Version.Interval.meet held.bounds wanting.bounds with
  | None -> Error (unsatisfied package { held with asked })
  | Some bounds -> settled state package { bounds; asked }

let noting ~asked_by state (package, range) =
  let wanting =
    { bounds = Version.Interval.of_range range; asked = [ (asked_by, range) ] }
  in
  match List.assoc_opt package state.needs with
  | None -> settled state package wanting
  | Some held -> merged state package held wanting

let rec noted ~asked_by state = function
  | [] -> Ok state
  | wanted :: rest ->
      Result.bind (noting ~asked_by state wanted) (fun widened ->
          noted ~asked_by widened rest)

let newest one other = Version.compare other one
let unchosen state (package, _) = not (List.mem_assoc package state.chosen)
let fitting state = List.find_opt (unchosen state) state.needs

let rec settle view state =
  match fitting state with
  | None -> Ok state.chosen
  | Some (package, need) -> pick view state package need

and pick view state package need =
  match view.versions package with
  | [] -> Error (Unknown_package { package; asked_by = asker need })
  | available ->
      let fits = List.filter (Version.Interval.holds need.bounds) available in
      tried view state package need (List.sort newest fits)

and tried view state package need = function
  | [] -> Error (unsatisfied package need)
  | version :: older ->
      weighed view state package need older (attempt view state package version)

and weighed view state package need older outcome =
  match outcome with
  | Ok answer -> Ok answer
  | Error problem ->
      Result.map_error (fun _ -> problem) (tried view state package need older)

and attempt view state package version =
  let taken = { state with chosen = (package, version) :: state.chosen } in
  Result.bind
    (noted ~asked_by:package taken (view.dependencies package version))
    (settle view)

let solved view wanted =
  let start = { needs = []; chosen = [] } in
  let listed chosen = List.sort Pick.by_name (List.map Pick.of_pair chosen) in
  Result.map listed
    (Result.bind (noted ~asked_by:root start wanted) (settle view))
