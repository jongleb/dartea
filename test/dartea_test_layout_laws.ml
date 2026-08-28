type style = {
  width : int;
  body_on_new_line : bool;
  arm_body_on_new_line : bool;
  comma_first : bool;
  if_on_new_lines : bool;
  signature_across_lines : bool;
  let_in_on_new_line : bool;
  let_bindings_on_new_line : bool;
  data_across_lines : bool;
}

type expr =
  | Int of int
  | Var of string
  | Unit
  | Text of string
  | Access of string * string list
  | App of string * expr list
  | Binop of string * expr * expr
  | Lambda of string list * expr
  | If of expr * expr * expr
  | Let of string * expr * expr
  | Record of (string * expr) list
  | Items of expr list
  | Case of string * (string * expr) list
  | Block_let of (string * expr) list * expr

type declaration =
  | Value of { name : string; signature : int option; body : expr }
  | Data of { name : string; constructors : string list }
  | Alias of { name : string; fields : (string * string) list }

type program = {
  header : (string * string list) option;
  imports : (string * string list) list;
  declarations : declaration list;
}

let rec inline = function
  | Int n -> string_of_int n
  | Var name -> name
  | Unit -> "()"
  | Text text -> "\"" ^ text ^ "\""
  | Access (root, fields) -> String.concat "." (root :: fields)
  | App (name, arguments) -> String.concat " " (name :: List.map atom arguments)
  | Binop (operator, left, right) ->
      Printf.sprintf "%s %s %s" (atom left) operator (atom right)
  | Lambda (parameters, body) ->
      Printf.sprintf "\\%s -> %s" (String.concat " " parameters) (inline body)
  | If (condition, yes, no) ->
      Printf.sprintf "if %s then %s else %s" (inline condition) (inline yes)
        (inline no)
  | Let (name, bound, body) ->
      Printf.sprintf "let %s = %s in %s" name (inline bound) (inline body)
  | Record fields ->
      "{ "
      ^ String.concat ", " (List.map (fun (k, v) -> k ^ " = " ^ inline v) fields)
      ^ " }"
  | Items items -> "[" ^ String.concat ", " (List.map inline items) ^ "]"
  | Case _ | Block_let _ -> invalid_arg "inline: block expression"

and atom expr =
  match expr with
  | Int _ | Var _ | Unit | Text _ | Access _ | Record _ | Items _ -> inline expr
  | _ -> "(" ^ inline expr ^ ")"

let pad width = String.make width ' '

let continue_first prefix = function
  | [] -> []
  | first :: rest -> (prefix ^ String.trim first) :: rest

let rec block expr ~indent ~style =
  match expr with
  | (Record _ | Items _) when style.comma_first -> comma_first expr ~indent
  | If (condition, yes, no) when style.if_on_new_lines ->
      [
        pad indent ^ "if " ^ inline condition;
        pad indent ^ "then " ^ inline yes;
        pad indent ^ "else " ^ inline no;
      ]
  | Case (scrutinee, arms) ->
      let arm_indent = indent + style.width in
      (pad indent ^ "case " ^ scrutinee ^ " of")
      :: List.concat_map
           (fun (pattern, body) ->
             match (body, style.arm_body_on_new_line) with
             | (Case _ | Block_let _), _ | _, true ->
                 (pad arm_indent ^ pattern ^ " ->")
                 :: block body ~indent:(arm_indent + style.width) ~style
             | _, false -> [ pad arm_indent ^ pattern ^ " -> " ^ inline body ])
           arms
  | Block_let (bindings, body) -> block_let bindings body ~indent ~style
  | expr -> [ pad indent ^ inline expr ]

and block_let bindings body ~indent ~style =
  let binding_line (name, bound) = name ^ " = " ^ inline bound in
  let body_lines = block body ~indent ~style in
  let tail = continue_first (pad indent ^ "in ") body_lines in
  match bindings with
  | [ binding ] when (not style.let_bindings_on_new_line) && not style.let_in_on_new_line
    ->
      continue_first
        (pad indent ^ "let " ^ binding_line binding ^ " in ")
        body_lines
  | [ binding ] when not style.let_bindings_on_new_line ->
      (pad indent ^ "let " ^ binding_line binding) :: tail
  | bindings ->
      ((pad indent ^ "let")
      :: List.map
           (fun binding -> pad (indent + style.width) ^ binding_line binding)
           bindings)
      @ tail

and comma_first expr ~indent =
  match expr with
  | Record fields ->
      layered ~opening:"{" ~closing:"}"
        (List.map (fun (k, v) -> k ^ " = " ^ inline v) fields)
        ~indent
  | Items items -> layered ~opening:"[" ~closing:"]" (List.map inline items) ~indent
  | _ -> invalid_arg "comma_first"

and layered ~opening ~closing items ~indent =
  match items with
  | [] -> [ pad indent ^ opening ^ closing ]
  | first :: rest ->
      ((pad indent ^ opening ^ " " ^ first)
       :: List.map (fun item -> pad indent ^ ", " ^ item) rest)
      @ [ pad indent ^ closing ]

let render_value ~name ~signature ~body ~style =
  let signature_lines =
    match signature with
    | None -> []
    | Some arrows ->
        let segments = List.init arrows (fun _ -> "-> Int") in
        if style.signature_across_lines then
          (name ^ ": Int")
          :: List.map (fun segment -> pad style.width ^ segment) segments
        else [ String.concat " " ((name ^ ": Int") :: segments) ]
  in
  let body_lines =
    match (body, style.body_on_new_line) with
    | (Case _ | Block_let _ | Record _ | Items _ | If _), _ | _, true ->
        (name ^ " =") :: block body ~indent:style.width ~style
    | body, false -> [ name ^ " = " ^ inline body ]
  in
  signature_lines @ body_lines

let render_declaration declaration ~style =
  match declaration with
  | Value { name; signature; body } -> render_value ~name ~signature ~body ~style
  | Data { name; constructors } ->
      if style.data_across_lines then
        (("type " ^ name)
        :: List.mapi
             (fun index constructor ->
               pad style.width
               ^ (if index = 0 then "= " else "| ")
               ^ constructor)
             constructors)
      else [ "type " ^ name ^ " = " ^ String.concat " | " constructors ]
  | Alias { name; fields } ->
      let rendered = List.map (fun (field, kind) -> field ^ ": " ^ kind) fields in
      if style.data_across_lines then
        ("type alias " ^ name ^ " =")
        :: layered ~opening:"{" ~closing:"}" rendered ~indent:style.width
      else [ "type alias " ^ name ^ " = { " ^ String.concat ", " rendered ^ " }" ]

let render program ~style =
  let header =
    match program.header with
    | None -> []
    | Some (name, exposed) ->
        [ "module " ^ name ^ " exposing (" ^ String.concat ", " exposed ^ ")" ]
  in
  let imports =
    List.map
      (fun (name, exposed) ->
        "import " ^ name ^ " exposing (" ^ String.concat ", " exposed ^ ")")
      program.imports
  in
  let declarations =
    List.map
      (fun declaration ->
        String.concat "\n" (render_declaration declaration ~style))
      program.declarations
  in
  "\n" ^ String.concat "\n\n" (header @ imports @ declarations) ^ "\n"

let brackets stream =
  List.filter
    (fun t -> t = Parse.Parser.INDENT || t = Parse.Parser.DEDENT)
    stream

let without_brackets stream =
  List.filter
    (fun t -> t <> Parse.Parser.INDENT && t <> Parse.Parser.DEDENT)
    stream

let balance stream =
  List.fold_left
    (fun (depth, lowest) token ->
      match token with
      | Parse.Parser.INDENT -> (depth + 1, lowest)
      | Parse.Parser.DEDENT -> (depth - 1, min lowest (depth - 1))
      | _ -> (depth, lowest))
    (0, 0) stream

open QCheck2

let name_gen = Gen.oneof_list [ "a"; "b"; "x"; "y"; "value"; "item" ]
let ctor_gen = Gen.oneof_list [ "A"; "B"; "Just"; "Nothing" ]
let type_name_gen = Gen.oneof_list [ "Color"; "User"; "Shape"; "Main" ]
let op_gen =
  Gen.oneof_list
    [ "+"; "-"; "*"; "//"; "^"; "=="; "&&"; "|>"; "<|"; "<<"; ">>"; "++"; "::" ]

let rec simple_gen depth =
  let leaf =
    Gen.oneof
      [
        Gen.map (fun n -> Int n) (Gen.int_range 0 99);
        Gen.map (fun name -> Var name) name_gen;
        Gen.return Unit;
        Gen.map (fun text -> Text text) (Gen.oneof_list [ "hi"; "hello world" ]);
        Gen.map2 (fun root field -> Access (root, [ field ])) name_gen name_gen;
      ]
  in
  if depth <= 0 then leaf
  else
    let smaller = simple_gen (depth - 1) in
    Gen.oneof_weighted
      [
        (4, leaf);
        (2, Gen.map2 (fun name arguments -> App (name, arguments)) name_gen
              (Gen.list_size (Gen.int_range 1 3) smaller));
        (2, Gen.map3 (fun operator left right -> Binop (operator, left, right))
              op_gen smaller smaller);
        (1, Gen.map2 (fun parameters body -> Lambda (parameters, body))
              (Gen.list_size (Gen.int_range 1 2) name_gen) smaller);
        (1, Gen.map3 (fun condition yes no -> If (condition, yes, no)) smaller
              smaller smaller);
        (1, Gen.map3 (fun name bound body -> Let (name, bound, body)) name_gen
              smaller smaller);
        (1, Gen.map (fun fields -> Record fields)
              (Gen.list_size (Gen.int_range 1 3) (Gen.pair name_gen smaller)));
        (1, Gen.map (fun items -> Items items)
              (Gen.list_size (Gen.int_range 1 3) smaller));
      ]

let pattern_gen =
  Gen.oneof
    [
      ctor_gen;
      Gen.return "_";
      name_gen;
      Gen.map2 (fun head tail -> head ^ " :: " ^ tail) name_gen name_gen;
      Gen.map2 (fun a b -> Printf.sprintf "[%s, %s]" a b) name_gen name_gen;
      Gen.map2 (fun a b -> Printf.sprintf "{%s, %s}" a b) name_gen name_gen;
      Gen.map2 (fun ctor argument -> ctor ^ " " ^ argument) ctor_gen name_gen;
    ]

let rec block_gen depth =
  if depth <= 0 then simple_gen 2
  else
    let smaller = block_gen (depth - 1) in
    Gen.oneof_weighted
      [
        (3, simple_gen 2);
        (3, Gen.map2 (fun scrutinee arms -> Case (scrutinee, arms)) name_gen
              (Gen.list_size (Gen.int_range 1 3) (Gen.pair pattern_gen smaller)));
        (2, Gen.map2 (fun bindings body -> Block_let (bindings, body))
              (Gen.list_size (Gen.int_range 1 3) (Gen.pair name_gen (simple_gen 2)))
              smaller);
      ]

let value_gen =
  Gen.map3
    (fun name signature body -> Value { name; signature; body })
    name_gen
    (Gen.option (Gen.int_range 0 2))
    (block_gen 2)

let data_gen =
  Gen.map2
    (fun name constructors -> Data { name; constructors })
    type_name_gen
    (Gen.list_size (Gen.int_range 1 3) ctor_gen)

let alias_gen =
  Gen.map2
    (fun name fields -> Alias { name; fields })
    type_name_gen
    (Gen.list_size (Gen.int_range 1 3)
       (Gen.pair name_gen (Gen.oneof_list [ "Int"; "String"; "Bool" ])))

let declaration_gen =
  Gen.oneof_weighted [ (5, value_gen); (2, data_gen); (2, alias_gen) ]

let exposing_gen = Gen.list_size (Gen.int_range 1 3) name_gen

let program_gen =
  let open Gen in
  let* header =
    option (pair type_name_gen exposing_gen)
  in
  let* imports =
    list_size (int_range 0 2) (pair type_name_gen exposing_gen)
  in
  let+ declarations = list_size (int_range 1 3) declaration_gen in
  { header; imports; declarations }

let style_gen =
  let open Gen in
  let* width = int_range 1 4 in
  let* body_on_new_line = bool in
  let* arm_body_on_new_line = bool in
  let* comma_first = bool in
  let* if_on_new_lines = bool in
  let* signature_across_lines = bool in
  let* let_in_on_new_line = bool in
  let* let_bindings_on_new_line = bool in
  let+ data_across_lines = bool in
  {
    width;
    body_on_new_line;
    arm_body_on_new_line;
    comma_first;
    if_on_new_lines;
    signature_across_lines;
    let_in_on_new_line;
    let_bindings_on_new_line;
    data_across_lines;
  }


let source_gen = Gen.map2 (fun program style -> render program ~style) program_gen style_gen
let print_source source = source

let law_parses =
  Test.make ~count:2000 ~name:"generated programs parse" ~print:print_source
    source_gen
    (fun source -> Result.is_ok (Parse.Main.parse ~file:"Main.elm" source))

let law_balanced =
  Test.make ~count:2000 ~name:"layout emits a balanced bracket word"
    ~print:print_source source_gen
    (fun source -> balance (Utils.layout_stream source) = (0, 0))

let law_blank_lines =
  Test.make ~count:2000 ~name:"blank lines do not change the layout"
    ~print:(fun (source, _, _) -> source)
    (Gen.triple source_gen Gen.nat_small (Gen.int_range 0 5))
    (fun (source, position, blank_width) ->
      let lines = String.split_on_char '\n' source in
      let index = position mod max 1 (List.length lines - 1) in
      let blank = String.make blank_width ' ' in
      let spliced =
        String.concat "\n"
          (List.concat
             (List.mapi
                (fun i line -> if i = index then [ blank; line ] else [ line ])
                lines))
      in
      Utils.layout_stream spliced = Utils.layout_stream source)

let law_comment_lines_ignored =
  Test.make ~count:2000 ~name:"a comment line does not change the layout"
    ~print:(fun (source, _, _) -> source)
    (Gen.triple source_gen Gen.nat_small (Gen.int_range 0 12))
    (fun (source, position, indent) ->
      let lines = String.split_on_char '\n' source in
      let index = position mod max 1 (List.length lines - 1) in
      let remark = String.make indent ' ' ^ "-- remark" in
      let spliced =
        String.concat "\n"
          (List.concat
             (List.mapi
                (fun i line -> if i = index then [ remark; line ] else [ line ])
                lines))
      in
      Utils.layout_stream spliced = Utils.layout_stream source)

let law_block_comments_ignored =
  Test.make ~count:2000 ~name:"a block comment does not change the layout"
    ~print:(fun (source, _, _) -> source)
    (Gen.triple source_gen Gen.nat_small (Gen.int_range 0 12))
    (fun (source, position, indent) ->
      let lines = String.split_on_char '\n' source in
      let index = position mod max 1 (List.length lines - 1) in
      let pad = String.make indent ' ' in
      let remark = pad ^ "{- a {- nested -} remark\n" ^ pad ^ "-}" in
      let spliced =
        String.concat "\n"
          (List.concat
             (List.mapi
                (fun i line -> if i = index then [ remark; line ] else [ line ])
                lines))
      in
      Utils.layout_stream spliced = Utils.layout_stream source)

let law_style_irrelevant =
  Test.make ~count:2000 ~name:"whitespace style does not change the token stream"
    ~print:(fun (program, left, right) ->
      let right = { right with let_bindings_on_new_line = left.let_bindings_on_new_line } in
      render program ~style:left ^ "\n----\n" ^ render program ~style:right)
    (Gen.triple program_gen style_gen style_gen)
    (fun (program, left, right) ->
      let right =
        { right with let_bindings_on_new_line = left.let_bindings_on_new_line }
      in
      Utils.layout_stream (render program ~style:left)
      = Utils.layout_stream (render program ~style:right))

let law_only_inserts_brackets =
  Test.make ~count:2000 ~name:"layout only inserts brackets"
    ~print:(fun (program, left, right) ->
      render program ~style:left ^ "\n----\n" ^ render program ~style:right)
    (Gen.triple program_gen style_gen style_gen)
    (fun (program, left, right) ->
      let stream style = Utils.layout_stream (render program ~style) in
      without_brackets (stream left) = without_brackets (stream right)
      && brackets (stream left) <> [])

let law_layout_idempotent =
  Test.make ~count:2000 ~name:"a repeated line break emits nothing"
    ~print:(fun (source, _) -> source)
    (Gen.pair source_gen (Gen.int_range 0 8))
    (fun (source, indent) ->
      let lexbuf = Lexing.from_string source in
      let rec go state =
        let _, settled = Parse.Indenter.layout state ~indent in
        fst (Parse.Indenter.layout settled ~indent) = []
        &&
        match Parse.Indenter.next_token state lexbuf with
        | Parse.Parser.EOF, _ -> true
        | _, state -> go state
      in
      go Parse.Indenter.initial)

let shift_indented_lines source ~by =
  String.concat "\n"
    (List.map
       (fun line ->
         if String.length line > 0 && line.[0] = ' ' then String.make by ' ' ^ line
         else line)
       (String.split_on_char '\n' source))

let law_uniform_shift =
  Test.make ~count:2000 ~name:"shifting every indented line keeps the stream"
    ~print:(fun (source, _) -> source)
    (Gen.pair source_gen (Gen.int_range 1 4))
    (fun (source, by) ->
      Utils.layout_stream (shift_indented_lines source ~by) = Utils.layout_stream source)

let raw_tokens input =
  let lexbuf = Lexing.from_string input in
  let rec go acc =
    match Parse.Lexer.token lexbuf with
    | Parse.Lexer.Token Parse.Parser.EOF -> List.rev acc
    | Parse.Lexer.Token token -> go (token :: acc)
    | _ -> go acc
  in
  go []

let bracket_depth tokens =
  List.fold_left
    (fun depth token ->
      match token with
      | Parse.Parser.INDENT -> depth + 1
      | Parse.Parser.DEDENT -> depth - 1
      | _ -> depth)
    0 tokens

let law_erases_to_raw_tokens =
  Test.make ~count:2000 ~name:"erasing brackets gives back the raw token stream"
    ~print:print_source source_gen
    (fun source -> without_brackets (Utils.layout_stream source) = raw_tokens source)

let law_emission_is_debt_change =
  Test.make ~count:2000 ~name:"emitted depth plus queued depth equals scope debt"
    ~print:print_source source_gen
    (fun source ->
      let lexbuf = Lexing.from_string source in
      let rec go state emitted =
        let open Parse.Indenter in
        emitted + bracket_depth state.pending = debt state.scopes
        &&
        match next_token state lexbuf with
        | Parse.Parser.EOF, state ->
            emitted = 0 && debt state.scopes = 0 && state.pending = []
        | token, state -> go state (emitted + bracket_depth [ token ])
      in
      go Parse.Indenter.initial 0)

let all_contexts =
  Parse.Indenter.
    [
      Top_level; Expression; Let; Let_binding; Let_inline; If; Case; Case_head;
      Case_arm; Type_alias; Type_decl; Type_annotation; Delimited;
    ]

let law_react_reads_only_the_offside =
  Test.make ~count:2000 ~name:"react depends only on the offside comparison"
    ~print:(fun (_, left, right) ->
      Printf.sprintf "%s vs %s"
        (String.concat "," (List.map string_of_int [ fst left; snd left ]))
        (String.concat "," (List.map string_of_int [ fst right; snd right ])))
    (Gen.triple
       (Gen.oneof_list all_contexts)
       (Gen.pair (Gen.int_range 1 20) (Gen.int_range 0 20))
       (Gen.pair (Gen.int_range 1 20) (Gen.int_range 0 20)))
    (fun (context, (left_indent, left_column), (right_indent, right_column)) ->
      let same_offside =
        compare left_indent left_column = compare right_indent right_column
      in
      (not same_offside)
      || Parse.Indenter.react context ~indent:left_indent ~column:left_column
         = Parse.Indenter.react context ~indent:right_indent
             ~column:right_column)

let run_with_snapshots source =
  let lexbuf = Lexing.from_string source in
  let rec go state acc =
    match Parse.Indenter.next_token state lexbuf with
    | Parse.Parser.EOF, _ -> List.rev acc
    | token, state -> go state ((token, state, lexbuf.Lexing.lex_curr_pos) :: acc)
  in
  go Parse.Indenter.initial []

let resume state source =
  let lexbuf = Lexing.from_string source in
  let rec go state acc =
    match Parse.Indenter.next_token state lexbuf with
    | Parse.Parser.EOF, _ -> List.rev acc
    | token, state -> go state (token :: acc)
  in
  go state []

let law_state_is_a_resumption_point =
  Test.make ~count:2000
    ~name:"lexing can restart from a saved state at a line break"
    ~print:(fun (source, _) -> source)
    (Gen.pair source_gen Gen.nat_small)
    (fun (source, choice) ->
      let steps = run_with_snapshots source in
      let breaks =
        List.filteri
          (fun index _ ->
            let _, _, position = List.nth steps index in
            position < String.length source && source.[position] = '\n')
          (List.mapi (fun index step -> (index, step)) steps)
      in
      match breaks with
      | [] -> true
      | breaks ->
          let index, (_, state, position) =
            List.nth breaks (choice mod List.length breaks)
          in
          let expected =
            List.filteri (fun position_in_run _ -> position_in_run > index) steps
            |> List.map (fun (token, _, _) -> token)
          in
          let rest = String.sub source position (String.length source - position) in
          resume state rest = expected)

let suite =
  QCheck_ounit.to_ounit2_test_list
    [
      law_parses;
      law_balanced;
      law_blank_lines;
      law_comment_lines_ignored;
      law_block_comments_ignored;
      law_style_irrelevant;
      law_only_inserts_brackets;
      law_layout_idempotent;
      law_uniform_shift;
      law_erases_to_raw_tokens;
      law_emission_is_debt_change;
      law_react_reads_only_the_offside;
      law_state_is_a_resumption_point;
    ]
