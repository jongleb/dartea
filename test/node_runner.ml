let read_all ic =
  let buffer = Buffer.create 256 in
  (try
     while true do
       Buffer.add_channel buffer ic 1
     done
   with End_of_file -> ());
  Buffer.contents buffer

let source path content = File_loader.Files.Elm_file.{ path; content }

let output_of (outcome : Dartea.Compiler.outcome) =
  match outcome.errors with
  | [] -> outcome.output
  | error :: _ -> raise (Reporting.Error.Found error)

let source_of ~module_name (compiled : Dartea.Compiler.compiled list) =
  List.filter_map
    (fun (c : Dartea.Compiler.compiled) ->
      if String.equal c.module_name module_name then Some c.source else None)
    compiled
  |> String.concat ""

let evaluate ~(compiled : Dartea.Compiler.compiled list) ~expr =
  let directory = Filename.temp_dir "dartea" "" in
  List.iter
    (fun (c : Dartea.Compiler.compiled) ->
      let file =
        Filename.concat directory
          (c.module_name ^ "." ^ Dartea.Compiler.extension)
      in
      let out = open_out file in
      output_string out c.source;
      close_out out)
    compiled;
  let program =
    Printf.sprintf
      "import * as Main from \"./Main.%s\"; console.log(JSON.stringify(%s));"
      Dartea.Compiler.extension expr
  in
  let command =
    Printf.sprintf "cd %s && node --input-type=module -e %s 2>&1"
      (Filename.quote directory) (Filename.quote program)
  in
  let ic = Unix.open_process_in command in
  let out = read_all ic in
  let (_ : Unix.process_status) = Unix.close_process_in ic in
  String.trim out

let index_of ~needle haystack =
  let needle_length = String.length needle in
  let last_start = String.length haystack - needle_length in
  let matches_at index = String.sub haystack index needle_length = needle in
  Seq.ints 0
  |> Seq.take_while (fun index -> index <= last_start)
  |> Seq.find matches_at

let contains ~needle haystack = index_of ~needle haystack <> None
