type block = { name : string; lines : string list }

type file = {
  preamble : string list;
  blocks : block list;
  exported : string list;
}

let identifier_char letter =
  (letter >= 'a' && letter <= 'z')
  || (letter >= 'A' && letter <= 'Z')
  || (letter >= '0' && letter <= '9')
  || letter = '_' || letter = '$'

type token = Name of string | Field of string

let tokens text =
  let word = Buffer.create 32 in
  let flush (found, dotted) =
    let token = Buffer.contents word in
    Buffer.clear word;
    if String.equal token "" then found
    else if dotted then Field token :: found
    else Name token :: found
  in
  let letter (found, dotted) letter =
    if identifier_char letter then begin
      Buffer.add_char word letter;
      (found, dotted)
    end
    else (flush (found, dotted), Char.equal letter '.')
  in
  List.rev (flush (String.fold_left letter ([], false) text))

let plain text =
  List.filter_map
    (function Name name -> Some name | Field _ -> None)
    (tokens text)

let opens line ~keyword = String.starts_with ~prefix:keyword line
let declares line = opens line ~keyword:"const "
let publishes line = opens line ~keyword:"export "
let name_of line = match plain line with _ :: name :: _ -> name | _ -> ""

let rec body kept = function
  | line :: _ as left when declares line || publishes line -> (List.rev kept, left)
  | line :: rest -> body (line :: kept) rest
  | [] -> (List.rev kept, [])

let rec gather found = function
  | line :: _ as left when publishes line -> (List.rev found, left)
  | line :: rest when declares line ->
      let lines, remaining = body [ line ] rest in
      gather ({ name = name_of line; lines } :: found) remaining
  | _ :: rest -> gather found rest
  | [] -> (List.rev found, [])

let parse source =
  let rec preamble_of kept = function
    | line :: _ as left when declares line -> (List.rev kept, left)
    | line :: rest -> preamble_of (line :: kept) rest
    | [] -> (List.rev kept, [])
  in
  let unexport = function
    | [] -> []
    | preamble_of :: rest ->
        let keyword = "export" in
        let body_line =
          if String.starts_with ~prefix:keyword preamble_of then
            String.sub preamble_of (String.length keyword)
              (String.length preamble_of - String.length keyword)
          else preamble_of
        in
        body_line :: rest
  in
  let preamble, rest = preamble_of [] (String.split_on_char '\n' source) in
  let blocks, tail = gather [] rest in
  { preamble; blocks; exported = List.concat_map plain (unexport tail) }

let mentions block spoken =
  let inside = List.concat_map plain block.lines in
  List.filter (fun name -> List.mem name inside) spoken

let reach file roots =
  let spoken = List.map (fun block -> block.name) file.blocks in
  let rec grown seen = function
    | [] -> seen
    | name :: rest when List.mem name seen -> grown seen rest
    | name :: rest -> (
        match List.find_opt (fun block -> block.name = name) file.blocks with
        | None -> grown (name :: seen) rest
        | Some block -> grown (name :: seen) (mentions block spoken @ rest))
  in
  grown [] roots

let alive ~roots source =
  let file = parse source in
  let alive_names = reach file roots in
  let survives name = List.mem name alive_names in
  String.concat "\n"
    (file.preamble
    @ List.concat_map
        (fun block -> if survives block.name then block.lines else [])
        file.blocks
    @ ("export {"
      :: List.filter_map
           (fun name -> if survives name then Some ("  " ^ name ^ ",") else None)
           file.exported)
    @ [ "};"; "" ])
