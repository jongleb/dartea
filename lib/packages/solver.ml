type view = {
  versions : string -> Version.t list;
  dependencies : string -> Version.t -> (string * Version.Range.t) list;
}

type problem =
  | Unknown_package of { package : string; asked_by : string }
  | No_version of { package : string; asked : (string * string) list }

type need = { bounds : Version.Interval.t; requests : (string * Version.Range.t) list }

type state = {
  needs : (string * need) list;
  chosen : (string * Version.t) list;
}

let root = "your dependencies"

let no_version package need =
  let shown (who, range) = (who, Version.Range.show range) in
  No_version { package; asked = List.map shown need.requests }

let asker need =
  match need.requests with [] -> root | (who, _) :: _ -> who

let swap ~package need (name, held) =
  if String.equal name package then (name, need) else (name, held)

let replace ~package need needs =
  match List.assoc_opt package needs with
  | None -> needs @ [ (package, need) ]
  | Some _ -> List.map (swap ~package need) needs

let settle state package need =
  match List.assoc_opt package state.chosen with
  | Some version when not (Version.Interval.holds need.bounds version) ->
      Error (no_version package need)
  | Some _ | None ->
      Ok { state with needs = replace ~package need state.needs }

let merge state package held demand =
  let requests = held.requests @ demand.requests in
  match Version.Interval.meet held.bounds demand.bounds with
  | None -> Error (no_version package { held with requests })
  | Some bounds -> settle state package { bounds; requests }

let note ~asked_by state (package, range) =
  let demand =
    { bounds = Version.Interval.of_range range; requests = [ (asked_by, range) ] }
  in
  match List.assoc_opt package state.needs with
  | None -> settle state package demand
  | Some held -> merge state package held demand

let rec note_all ~asked_by state = function
  | [] -> Ok state
  | wanted :: rest ->
      Result.bind (note ~asked_by state wanted) (fun widened ->
          note_all ~asked_by widened rest)

let newest one other = Version.compare other one
let unchosen state (package, _) = not (List.mem_assoc package state.chosen)
let next_unchosen state = List.find_opt (unchosen state) state.needs

let rec settle view state =
  match next_unchosen state with
  | None -> Ok state.chosen
  | Some (package, need) -> pick view state package need

and pick view state package need =
  match view.versions package with
  | [] -> Error (Unknown_package { package; asked_by = asker need })
  | available ->
      let fits = List.filter (Version.Interval.holds need.bounds) available in
      try_each view state package need (List.sort newest fits)

and try_each view state package need = function
  | [] -> Error (no_version package need)
  | version :: older ->
      weigh view state package need older (attempt view state package version)

and weigh view state package need older outcome =
  match outcome with
  | Ok answer -> Ok answer
  | Error problem ->
      Result.map_error (fun _ -> problem) (try_each view state package need older)

and attempt view state package version =
  let taken = { state with chosen = (package, version) :: state.chosen } in
  Result.bind
    (note_all ~asked_by:package taken (view.dependencies package version))
    (settle view)

let solve view wanted =
  let start = { needs = []; chosen = [] } in
  let picks_of chosen = List.sort Pick.by_name (List.map Pick.of_pair chosen) in
  Result.map picks_of
    (Result.bind (note_all ~asked_by:root start wanted) (settle view))
