
type style = {
  width : int;
  body_on_new_line : bool;
  arm_body_on_new_line : bool;
  comma_first : bool;
  if_on_new_lines : bool;
  signature_across_lines : bool;
}

type expr =
  | Int of int
  | Var of string
  | App of string * expr list
  | Binop of string * expr * expr
  | If of expr * expr * expr
  | Let of string * expr * expr
  | Record of (string * expr) list
  | Items of expr list
  | Case of string * (string * expr) list

type decl = { name : string; signature : int option; body : expr }

let rec inline = function
  | Int n -> string_of_int n
  | Var v -> v
  | App (f, args) -> String.concat " " (f :: List.map atom args)
  | Binop (op, left, right) ->
      Printf.sprintf "%s %s %s" (atom left) op (atom right)
  | If (cond, yes, no) ->
      Printf.sprintf "if %s then %s else %s" (inline cond) (inline yes)
        (inline no)
  | Let (name, rhs, body) ->
      Printf.sprintf "let %s = %s in %s" name (inline rhs) (inline body)
  | Record fields ->
      "{ "
      ^ String.concat ", "
          (List.map (fun (k, v) -> k ^ " = " ^ inline v) fields)
      ^ " }"
  | Items items -> "[" ^ String.concat ", " (List.map inline items) ^ "]"
  | Case _ -> invalid_arg "inline: case is a block"

and atom expr =
  match expr with
  | Int _ | Var _ | Record _ | Items _ -> inline expr
  | _ -> "(" ^ inline expr ^ ")"

let pad width = String.make width ' '

let rec block expr ~indent ~style =
  match expr with
  | (Record _ | Items _) when style.comma_first -> comma_first expr ~indent
  | If (cond, yes, no) when style.if_on_new_lines ->
      [
        pad indent ^ "if " ^ inline cond;
        pad indent ^ "then " ^ inline yes;
        pad indent ^ "else " ^ inline no;
      ]
  | Case (scrutinee, arms) ->
      let arm_indent = indent + style.width in
      (pad indent ^ "case " ^ scrutinee ^ " of")
      :: List.concat_map
           (fun (pattern, body) ->
             match (body, style.arm_body_on_new_line) with
             | Case _, _ | _, true ->
                 (pad arm_indent ^ pattern ^ " ->")
                 :: block body ~indent:(arm_indent + style.width) ~style
             | _, false -> [ pad arm_indent ^ pattern ^ " -> " ^ inline body ])
           arms
  | expr -> [ pad indent ^ inline expr ]

and comma_first expr ~indent =
  let opening, closing, items =
    match expr with
    | Record fields ->
        ("{", "}", List.map (fun (k, v) -> k ^ " = " ^ inline v) fields)
    | Items items -> ("[", "]", List.map inline items)
    | _ -> invalid_arg "comma_first"
  in
  let first, rest =
    match items with [] -> ("", []) | first :: rest -> (first, rest)
  in
  ((pad indent ^ opening ^ " " ^ first)
   :: List.map (fun item -> pad indent ^ ", " ^ item) rest)
  @ [ pad indent ^ closing ]

let render_decl decl ~style =
  let signature =
    match decl.signature with
    | None -> []
    | Some arrows ->
        let arrow_segments = List.init arrows (fun _ -> "-> Int") in
        if style.signature_across_lines then
          (decl.name ^ ": Int")
          :: List.map (fun segment -> pad style.width ^ segment) arrow_segments
        else [ String.concat " " ((decl.name ^ ": Int") :: arrow_segments) ]
  in
  let body =
    match (decl.body, style.body_on_new_line) with
    | (Case _ | Record _ | Items _ | If _), _ | _, true ->
        (decl.name ^ " =") :: block decl.body ~indent:style.width ~style
    | body, false -> [ decl.name ^ " = " ^ inline body ]
  in
  signature @ body

let render program ~style =
  "\n"
  ^ String.concat "\n\n" (List.map (fun d -> String.concat "\n" (render_decl d ~style)) program)
  ^ "\n"

let layout_stream input =
  let lexbuf = Lexing.from_string input in
  let rec go state acc =
    match Parse.Indenter.next_token state lexbuf with
    | Parse.Parser.EOF, _ -> List.rev acc
    | token, state -> go state (token :: acc)
  in
  go Parse.Indenter.initial []

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
let op_gen = Gen.oneof_list [ "+"; "-"; "*"; "=="; "&&"; "|>"; "++" ]

let rec simple_gen depth =
  let leaf =
    Gen.oneof
      [ Gen.map (fun n -> Int n) (Gen.int_range 0 99);
        Gen.map (fun v -> Var v) name_gen ]
  in
  if depth <= 0 then leaf
  else
    Gen.oneof_weighted
      [
        (3, leaf);
        ( 2,
          Gen.map2
            (fun f args -> App (f, args))
            name_gen
            (Gen.list_size (Gen.int_range 1 3) (simple_gen (depth - 1))) );
        ( 2,
          Gen.map3
            (fun op l r -> Binop (op, l, r))
            op_gen (simple_gen (depth - 1)) (simple_gen (depth - 1)) );
        ( 1,
          Gen.map3
            (fun c y n -> If (c, y, n))
            (simple_gen (depth - 1)) (simple_gen (depth - 1))
            (simple_gen (depth - 1)) );
        ( 1,
          Gen.map3
            (fun x rhs body -> Let (x, rhs, body))
            name_gen (simple_gen (depth - 1)) (simple_gen (depth - 1)) );
        ( 1,
          Gen.map
            (fun fields -> Record fields)
            (Gen.list_size (Gen.int_range 1 3)
               (Gen.pair name_gen (simple_gen (depth - 1)))) );
        ( 1,
          Gen.map
            (fun items -> Items items)
            (Gen.list_size (Gen.int_range 1 3) (simple_gen (depth - 1))) );
      ]

let pattern_gen =
  Gen.oneof
    [
      ctor_gen;
      Gen.return "_";
      name_gen;
      Gen.map2 (fun h t -> h ^ " :: " ^ t) name_gen name_gen;
      Gen.map2 (fun a b -> Printf.sprintf "[%s, %s]" a b) name_gen name_gen;
      Gen.map2 (fun a b -> Printf.sprintf "{%s, %s}" a b) name_gen name_gen;
      Gen.map2 (fun c a -> c ^ " " ^ a) ctor_gen name_gen;
    ]

let rec block_gen depth =
  if depth <= 0 then simple_gen 2
  else
    Gen.oneof_weighted
      [
        (2, simple_gen 2);
        ( 3,
          Gen.map2
            (fun scrutinee arms -> Case (scrutinee, arms))
            name_gen
            (Gen.list_size (Gen.int_range 1 3)
               (Gen.pair pattern_gen (block_gen (depth - 1)))) );
      ]

let decl_gen =
  Gen.map3
    (fun name signature body -> { name; signature; body })
    name_gen
    (Gen.option (Gen.int_range 0 2))
    (block_gen 2)

let program_gen = Gen.list_size (Gen.int_range 1 3) decl_gen

let style_gen =
  let open Gen in
  let* width = int_range 1 4 in
  let* body_on_new_line = bool in
  let* arm_body_on_new_line = bool in
  let* comma_first = bool in
  let* if_on_new_lines = bool in
  let+ signature_across_lines = bool in
  {
    width;
    body_on_new_line;
    arm_body_on_new_line;
    comma_first;
    if_on_new_lines;
    signature_across_lines;
  }

let source_gen = Gen.map2 (fun program style -> render program ~style) program_gen style_gen
let print_source source = source

let law_parses =
  Test.make ~count:500 ~name:"generated programs parse" ~print:print_source
    source_gen
    (fun source -> Result.is_ok (Parse.Main.parse source))

let law_balanced =
  Test.make ~count:500 ~name:"layout emits a balanced bracket word"
    ~print:print_source source_gen
    (fun source -> balance (layout_stream source) = (0, 0))

let law_blank_lines =
  Test.make ~count:300 ~name:"blank lines do not change the layout"
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
      layout_stream spliced = layout_stream source)

let law_style_irrelevant =
  Test.make ~count:300 ~name:"layout style does not change the token stream"
    ~print:(fun (program, left, right) ->
      render program ~style:left ^ "\n----\n" ^ render program ~style:right)
    (Gen.triple program_gen style_gen style_gen)
    (fun (program, left, right) ->
      layout_stream (render program ~style:left)
      = layout_stream (render program ~style:right))

let law_only_inserts_brackets =
  Test.make ~count:300 ~name:"layout only inserts brackets"
    ~print:(fun (program, left, right) ->
      render program ~style:left ^ "\n----\n" ^ render program ~style:right)
    (Gen.triple program_gen style_gen style_gen)
    (fun (program, left, right) ->
      let stream style = layout_stream (render program ~style) in
      without_brackets (stream left) = without_brackets (stream right)
      && brackets (stream left) <> [])

let scope_columns state =
  List.map
    (fun (scope : Parse.Indenter.scope) -> scope.Parse.Indenter.column)
    state.Parse.Indenter.scopes

let rec non_increasing = function
  | first :: (second :: _ as rest) -> first >= second && non_increasing rest
  | _ -> true

let law_monotone_scopes =
  Test.make ~count:300 ~name:"scope columns never decrease toward the top"
    ~print:print_source source_gen
    (fun source ->
      let lexbuf = Lexing.from_string source in
      let rec go state =
        non_increasing (scope_columns state)
        &&
        match Parse.Indenter.next_token state lexbuf with
        | Parse.Parser.EOF, state -> non_increasing (scope_columns state)
        | _, state -> go state
      in
      go Parse.Indenter.initial)

let law_layout_idempotent =
  Test.make ~count:300 ~name:"a repeated line break emits nothing"
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
  Test.make ~count:300 ~name:"shifting every indented line keeps the stream"
    ~print:(fun (source, _) -> source)
    (Gen.pair source_gen (Gen.int_range 1 4))
    (fun (source, by) ->
      layout_stream (shift_indented_lines source ~by) = layout_stream source)

let suite =
  QCheck_ounit.to_ounit2_test_list
    [
      law_parses;
      law_balanced;
      law_blank_lines;
      law_style_irrelevant;
      law_only_inserts_brackets;
      law_monotone_scopes;
      law_layout_idempotent;
      law_uniform_shift;
    ]
