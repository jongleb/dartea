type report = {
  declarations : int;
  live : int;
  bytes : int;
  live_bytes : int;
}

type module_ = {
  aliases : (string * string) list;
  declarations : (string * string) list;
}

let import_line =
  Str.regexp {|import \* as \([A-Za-z0-9_$]+\) from "\./\(.+\)\.mjs";|}

let parsed source =
  let aliases =
    List.filter_map
      (fun line ->
        if Str.string_match import_line line 0 then
          Some (Str.matched_group 1 line, Str.matched_group 2 line)
        else None)
      (String.split_on_char '\n' source)
  in
  let declarations =
    List.map
      (fun (block : Codegen_js.Shake.block) ->
        (block.name, String.concat "\n" block.lines))
      (Codegen_js.Shake.parse source).blocks
  in
  { aliases; declarations }

let references ~home module_ body =
  let own = List.map fst module_.declarations in
  let rec walked found = function
    | Codegen_js.Shake.Name word :: Codegen_js.Shake.Field member :: rest
      when List.mem_assoc word module_.aliases ->
        walked ((List.assoc word module_.aliases, member) :: found) rest
    | Codegen_js.Shake.Name word :: rest when List.mem word own ->
        walked ((home, word) :: found) rest
    | _ :: rest -> walked found rest
    | [] -> found
  in
  walked [] (Codegen_js.Shake.tokens body)

let library =
  List.map Prelude.name Prelude.all
  @ (Codegen_js.Of_optimized.runtime_module_name
    :: Codegen_js.Platform_kernel.module_names)

let measure modules =
  let parsed_modules =
    List.map (fun (name, source) -> (name, parsed source)) modules
  in
  let declarations_of home =
    match List.assoc_opt home parsed_modules with
    | Some module_ -> module_.declarations
    | None -> []
  in
  let roots =
    List.concat_map
      (fun (name, module_) ->
        if List.mem name library then []
        else List.map (fun (own, _) -> (name, own)) module_.declarations)
      parsed_modules
  in
  let rec grown seen = function
    | [] -> seen
    | (home, name) :: rest when List.mem (home, name) seen -> grown seen rest
    | (home, name) :: rest -> (
        match List.assoc_opt name (declarations_of home) with
        | None -> grown seen rest
        | Some body ->
            let module_ = List.assoc home parsed_modules in
            grown
              ((home, name) :: seen)
              (references ~home module_ body @ rest))
  in
  let alive = grown [] roots in
  let weight (_, body) = String.length body in
  {
    declarations =
      List.fold_left
        (fun total (_, module_) -> total + List.length module_.declarations)
        0 parsed_modules;
    live = List.length alive;
    bytes =
      List.fold_left
        (fun total (_, module_) ->
          List.fold_left (fun sum entry -> sum + weight entry) total
            module_.declarations)
        0 parsed_modules;
    live_bytes =
      List.fold_left
        (fun total (home, name) ->
          total + String.length (List.assoc name (declarations_of home)))
        0 alive;
  }

let shown name report =
  Printf.sprintf "%s %d/%d declarations, %d/%d bytes" name report.live
    report.declarations report.live_bytes report.bytes
