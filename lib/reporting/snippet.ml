let width_of_numbers last = String.length (string_of_int last)

let numbered ~width number line =
  let written = string_of_int number in
  Doc.text (String.make (width - String.length written) ' ' ^ written ^ "| " ^ line)

let underline ~width (region : Data.Region.t) =
  let leading = String.make (width + 2 + (region.start.column - 1)) ' ' in
  let marks = max 1 (region.stop.column - region.start.column) in
  Doc.beside [ Doc.text leading; Doc.red (Doc.text (String.make marks '^')) ]

let pointed ~width number line =
  let written = string_of_int number in
  Doc.beside
    [
      Doc.text (String.make (width - String.length written) ' ' ^ written ^ "|");
      Doc.red (Doc.text ">");
      Doc.text line;
    ]

let of_region source (region : Data.Region.t) =
  let width = width_of_numbers region.stop.line in
  let numbers =
    List.init (region.stop.line - region.start.line + 1) (fun step ->
        region.start.line + step)
  in
  let shown =
    List.filter_map
      (fun number ->
        Option.map (fun line -> (number, line)) (Source.line_at source number))
      numbers
  in
  let rec without_trailing_blanks lines =
    match List.rev lines with
    | (_, last) :: earlier when String.equal (String.trim last) "" && not (List.is_empty earlier) ->
        without_trailing_blanks (List.rev earlier)
    | _ -> lines
  in
  let shown = without_trailing_blanks shown in
  if List.length shown = 0 then Doc.empty
  else if Data.Region.spans_one_line region then
    Doc.above
      (List.map (fun (number, line) -> numbered ~width number line) shown
      @ [ underline ~width region ])
  else Doc.above (List.map (fun (number, line) -> pointed ~width number line) shown)
