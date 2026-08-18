open OUnit2

let mutating =
  [ "Variable.link"; "Variable.constrain"; "Variable.lower_to";
    "Variable.enter_level"; "Variable.leave_level" ]

let owns_the_mutation path =
  String.equal path "../lib/ast/typed/variable.ml"
  || String.equal path "../lib/ast/typed/variable.mli"
  || String.starts_with ~prefix:"../lib/infer/" path

let rec sources found path =
  if Sys.is_directory path then
    Array.fold_left
      (fun found entry -> sources found (Filename.concat path entry))
      found (Sys.readdir path)
  else if Filename.check_suffix path ".ml" || Filename.check_suffix path ".mli"
  then path :: found
  else found

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let identifier_char letter =
  match letter with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

let calls ~needle content =
  let taken = String.length needle and whole = String.length content in
  let rec from index =
    if index + taken > whole then false
    else if String.equal (String.sub content index taken) needle
            && (index + taken >= whole
               || not (identifier_char content.[index + taken]))
    then true
    else from (index + 1)
  in
  from 0

let calls_a_mutation path =
  let content = read path in
  List.exists (fun needle -> calls ~needle content) mutating

let test_only_inference_can_bind_a_variable _ =
  let outside =
    sources [] "../lib"
    |> List.filter (fun path -> not (owns_the_mutation path))
    |> List.filter calls_a_mutation
  in
  assert_equal ~printer:(String.concat ", ") [] outside

let test_the_scan_looks_at_the_inference_layer _ =
  let inside =
    sources [] "../lib" |> List.filter owns_the_mutation
    |> List.filter calls_a_mutation
  in
  assert_bool "the mutating calls are found where they do live" (inside <> [])

let suite =
  [
    "only inference can bind a variable"
    >:: test_only_inference_can_bind_a_variable;
    "the scan looks at the inference layer"
    >:: test_the_scan_looks_at_the_inference_layer;
  ]
