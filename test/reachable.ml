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

let identifier_char letter =
  (letter >= 'a' && letter <= 'z')
  || (letter >= 'A' && letter <= 'Z')
  || (letter >= '0' && letter <= '9')
  || letter = '_' || letter = '$'

let words text =
  let length = String.length text in
  let rec gathered index found =
    if index >= length then List.rev found
    else if identifier_char text.[index] then begin
      let stop = ref index in
      while !stop < length && identifier_char text.[!stop] do
        incr stop
      done;
      let dotted = index > 0 && text.[index - 1] = '.' in
      let word = String.sub text index (!stop - index) in
      gathered !stop ((word, dotted) :: found)
    end
    else gathered (index + 1) found
  in
  gathered 0 []

let starts_with ~prefix line =
  String.length line >= String.length prefix
  && String.equal (String.sub line 0 (String.length prefix)) prefix

let import_line =
  Str.regexp {|import \* as \([A-Za-z0-9_$]+\) from "\./\(.+\)\.mjs";|}

let named line =
  let start = String.length "const " in
  let stop = ref start in
  while !stop < String.length line && identifier_char line.[!stop] do
    incr stop
  done;
  String.sub line start (!stop - start)

let parsed source =
  let lines = String.split_on_char '\n' source in
  let aliases =
    List.filter_map
      (fun line ->
        if Str.string_match import_line line 0 then
          Some (Str.matched_group 1 line, Str.matched_group 2 line)
        else None)
      lines
  in
  let rec gathered found = function
    | [] -> List.rev found
    | line :: rest when starts_with ~prefix:"const " line ->
        let body, remaining =
          let rec taken kept = function
            | next :: _ as left
              when starts_with ~prefix:"const " next
                   || starts_with ~prefix:"export " next ->
                (List.rev kept, left)
            | next :: left -> taken (next :: kept) left
            | [] -> (List.rev kept, [])
          in
          taken [ line ] rest
        in
        gathered ((named line, String.concat "\n" body) :: found) remaining
    | _ :: rest -> gathered found rest
  in
  { aliases; declarations = gathered [] lines }

let references ~home module_ body =
  let own = List.map fst module_.declarations in
  let rec walked found = function
    | (word, _) :: (member, true) :: rest
      when List.mem_assoc word module_.aliases ->
        walked ((List.assoc word module_.aliases, member) :: found) rest
    | (word, false) :: rest when List.mem word own ->
        walked ((home, word) :: found) rest
    | _ :: rest -> walked found rest
    | [] -> found
  in
  walked [] (words body)

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
