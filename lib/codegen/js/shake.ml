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
  let ended (found, dotted) =
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
    else (ended (found, dotted), Char.equal letter '.')
  in
  List.rev (ended (String.fold_left letter ([], false) text))

let plain text =
  List.filter_map
    (function Name name -> Some name | Field _ -> None)
    (tokens text)

let opens line ~keyword = String.starts_with ~prefix:keyword line
let declares line = opens line ~keyword:"const "
let publishes line = opens line ~keyword:"export "
let named line = match plain line with _ :: name :: _ -> name | _ -> ""

let rec body kept = function
  | line :: _ as left when declares line || publishes line -> (List.rev kept, left)
  | line :: rest -> body (line :: kept) rest
  | [] -> (List.rev kept, [])

let rec gathered found = function
  | line :: _ as left when publishes line -> (List.rev found, left)
  | line :: rest when declares line ->
      let lines, remaining = body [ line ] rest in
      gathered ({ name = named line; lines } :: found) remaining
  | _ :: rest -> gathered found rest
  | [] -> (List.rev found, [])

let parsed source =
  let rec opening kept = function
    | line :: _ as left when declares line -> (List.rev kept, left)
    | line :: rest -> opening (line :: kept) rest
    | [] -> (List.rev kept, [])
  in
  let listed = function
    | [] -> []
    | opening :: rest ->
        let keyword = "export" in
        let unexported =
          if String.starts_with ~prefix:keyword opening then
            String.sub opening (String.length keyword)
              (String.length opening - String.length keyword)
          else opening
        in
        unexported :: rest
  in
  let preamble, rest = opening [] (String.split_on_char '\n' source) in
  let blocks, tail = gathered [] rest in
  { preamble; blocks; exported = List.concat_map plain (listed tail) }

let mentions block spoken =
  let inside = List.concat_map plain block.lines in
  List.filter (fun name -> List.mem name inside) spoken

let reached file roots =
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
  let file = parsed source in
  let living = reached file roots in
  let surviving name = List.mem name living in
  String.concat "\n"
    (file.preamble
    @ List.concat_map
        (fun block -> if surviving block.name then block.lines else [])
        file.blocks
    @ ("export {"
      :: List.filter_map
           (fun name -> if surviving name then Some ("  " ^ name ^ ",") else None)
           file.exported)
    @ [ "};"; "" ])
