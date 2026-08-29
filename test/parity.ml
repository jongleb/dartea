let identifier_char letter =
  (letter >= 'a' && letter <= 'z')
  || (letter >= 'A' && letter <= 'Z')
  || (letter >= '0' && letter <= '9')
  || letter = '_' || letter = '.'

let opening source =
  let wanted = "exposing" in
  let length = String.length source in
  let rec found index =
    if index + String.length wanted > length then None
    else if String.equal (String.sub source index (String.length wanted)) wanted
    then
      let rec bracket at =
        if at >= length then None
        else if source.[at] = '(' then Some (at + 1)
        else if source.[at] = ' ' || source.[at] = '\n' then bracket (at + 1)
        else None
      in
      match bracket (index + String.length wanted) with
      | Some at -> Some at
      | None -> found (index + 1)
    else found (index + 1)
  in
  found 0

let listed source =
  match opening source with
  | None -> []
  | Some start ->
      let length = String.length source in
      let rec taken index depth gathered token =
        if index >= length then List.rev gathered
        else
          let letter = source.[index] in
          let kept spelled =
            let trimmed = String.trim spelled in
            if String.equal trimmed "" then gathered else trimmed :: gathered
          in
          match letter with
          | ')' when depth = 0 -> List.rev (kept token)
          | ')' -> taken (index + 1) (depth - 1) gathered (token ^ ")")
          | '(' -> taken (index + 1) (depth + 1) gathered (token ^ "(")
          | ',' when depth = 0 -> taken (index + 1) depth (kept token) ""
          | '\n' -> taken (index + 1) depth gathered (token ^ " ")
          | _ -> taken (index + 1) depth gathered (token ^ String.make 1 letter)
      in
      taken start 0 [] ""

let bare name =
  let suffix = "(..)" in
  let length = String.length name in
  if
    length >= String.length suffix
    && String.equal
         (String.sub name (length - String.length suffix) (String.length suffix))
         suffix
  then String.trim (String.sub name 0 (length - String.length suffix))
  else name

let names source =
  List.sort_uniq String.compare
    (List.filter
       (fun name -> String.exists identifier_char name || String.length name > 0)
       (List.map bare (listed source)))
