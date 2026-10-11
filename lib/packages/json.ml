let decode ~file of_yojson content =
  let failed ~file message =
    if String.contains message ' ' then
      Diagnostic.Failure.raise_project (Bad_value { file; expected = message })
    else
      let path = String.split_on_char '.' message in
      let field = List.nth path (List.length path - 1) in
      Diagnostic.Failure.raise_project
        (Bad_field
           {
             file;
             field =
               String.map
                 (function '_' -> '-' | letter -> letter)
                 (if String.ends_with ~suffix:"_" field then
                    String.sub field 0 (String.length field - 1)
                  else field);
           })
  in
  let flatten problem =
    String.map (function '\n' | '\r' | '\t' -> ' ' | letter -> letter) problem
  in
  match Yojson.Safe.from_string content with
  | `Assoc _ as json -> Result.fold ~ok:Fun.id ~error:(failed ~file) (of_yojson json)
  | _ ->
      Diagnostic.Failure.raise_project
        (Bad_json { file; problem = "I was expecting a JSON object here." })
  | exception Yojson.Json_error problem ->
      Diagnostic.Failure.raise_project
        (Bad_json { file; problem = flatten problem })

let pairs item = function
  | `Assoc rows ->
      List.fold_right
        (fun (name, held) gathered ->
          Result.bind (item held) (fun value ->
              Result.map (fun rest -> (name, value) :: rest) gathered))
        rows (Ok [])
  | _ -> Error "an object of package names and versions"

let nonempty items =
  let expected = "a non-empty array of " ^ items in
  function
  | `List held ->
      let names =
        List.filter_map (function `String name -> Some name | _ -> None) held
      in
      if List.compare_lengths names held = 0 then
        Option.to_result ~none:expected (Non_empty_list.of_list names)
      else Error expected
  | _ -> Error expected

let load root path decode =
  if Files.is_file root path then
    Some (decode ~file:(Files.at root path) (Files.load root path))
  else None
