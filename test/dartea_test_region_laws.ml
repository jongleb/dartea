open OUnit2
open QCheck2
module R = Data.Region

let regions source = Utils.regions_of (Utils.parsed source)

let recounted source offset =
  let rec walk position line bol =
    if position >= offset then (line, position - bol + 1)
    else if source.[position] = '\n' then walk (position + 1) (line + 1) (position + 1)
    else walk (position + 1) line bol
  in
  walk 0 1 0

let law_regions_stay_inside_the_source =
  Test.make ~count:1000 ~name:"a region lies inside the source it came from"
    ~print:Fun.id Dartea_test_layout_laws.source_gen
    (fun source ->
      List.for_all
        (fun (region : R.t) ->
          0 <= region.start.offset
          && region.start.offset <= region.stop.offset
          && region.stop.offset <= String.length source)
        (regions source))

let law_line_and_column_follow_the_offset =
  Test.make ~count:1000
    ~name:"line and column agree with a recount from the offset" ~print:Fun.id
    Dartea_test_layout_laws.source_gen
    (fun source ->
      List.for_all
        (fun (region : R.t) ->
          recounted source region.start.offset = (region.start.line, region.start.column)
          && recounted source region.stop.offset
             = (region.stop.line, region.stop.column))
        (regions source))

let law_every_region_names_its_file =
  Test.make ~count:1000 ~name:"a region names the file it was Utils.parsed from"
    ~print:Fun.id Dartea_test_layout_laws.source_gen
    (fun source ->
      List.for_all
        (fun (region : R.t) -> String.equal region.file "Main.elm")
        (regions source))

let law_underlined_text_is_not_empty =
  Test.make ~count:1000 ~name:"the text a region underlines is not empty"
    ~print:Fun.id Dartea_test_layout_laws.source_gen
    (fun source ->
      List.for_all
        (fun (region : R.t) -> region.stop.offset > region.start.offset)
        (regions source))

let slice source (region : R.t) =
  String.sub source region.start.offset (region.stop.offset - region.start.offset)

let expression source name =
  List.find_map
    (function
      | Frontend.Impl.Top_declaration (d : Frontend.Declaration.t)
        when String.equal (Data.Located.unwrap d.body_part.name) name ->
          Some d.body_part.expr
      | _ -> None)
    (Utils.parsed source)
  |> Option.get

let assert_slice ~expected source (expr : Frontend.Expr.t) =
  assert_equal ~printer:Fun.id expected (slice source expr.region)

let test_an_argument_is_underlined _ =
  let source = "f : Int -> Int\nf n = n\n\nbad = f \"text\"\n" in
  match (expression source "bad").thing with
  | Frontend.Expr.Expr_apply { fn; arg } ->
      assert_slice ~expected:"f" source fn;
      assert_slice ~expected:"\"text\"" source arg;
      assert_slice ~expected:"f \"text\"" source (expression source "bad")
  | _ -> assert_failure "bad is not an application"

let test_a_branch_is_underlined _ =
  let source = "answer m =\n    case m of\n        Just x ->\n            x\n" in
  match (expression source "answer").thing with
  | Frontend.Expr.Expr_pattern { expr; pattern_data_items = [ branch ] } ->
      assert_slice ~expected:"m" source expr;
      assert_equal ~printer:Fun.id "Just x" (slice source branch.pattern.region);
      assert_slice ~expected:"x" source branch.expr
  | _ -> assert_failure "answer is not a case expression"

let test_an_operand_is_underlined _ =
  let source = "total a =\n    a + 1\n" in
  match (expression source "total").thing with
  | Frontend.Expr.Expr_binop { operands = left, right; _ } ->
      assert_slice ~expected:"a" source left;
      assert_slice ~expected:"1" source right
  | _ -> assert_failure "total is not a binary operation"

let test_a_region_spans_several_lines _ =
  let source = "pick c =\n    if c then\n        1\n\n    else\n        2\n" in
  let region = (expression source "pick").region in
  assert_equal ~printer:string_of_int 2 region.start.line;
  assert_equal ~printer:string_of_int 6 region.stop.line;
  assert_bool "spans more than one line" (not (R.spans_one_line region))

let suite =
  [
    "an argument is underlined" >:: test_an_argument_is_underlined;
    "a case branch is underlined" >:: test_a_branch_is_underlined;
    "an operand is underlined" >:: test_an_operand_is_underlined;
    "a region spans several lines" >:: test_a_region_spans_several_lines;
  ]
  @ List.map QCheck_ounit.to_ounit2_test
      [
        law_regions_stay_inside_the_source;
        law_line_and_column_follow_the_offset;
        law_every_region_names_its_file;
        law_underlined_text_is_not_empty;
      ]
