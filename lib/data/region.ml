type position = { offset : int; line : int; column : int }
type t = { file : string; start : position; stop : position }

let position_of_lexing (position : Lexing.position) =
  {
    offset = position.pos_cnum;
    line = position.pos_lnum;
    column = (position.pos_cnum - position.pos_bol) + 1;
  }

let of_lexing (start, stop) =
  {
    file = start.Lexing.pos_fname;
    start = position_of_lexing start;
    stop = position_of_lexing stop;
  }

let nowhere =
  let origin = { offset = 0; line = 1; column = 1 } in
  { file = ""; start = origin; stop = origin }

let merge one other =
  let start =
    if one.start.offset <= other.start.offset then one.start else other.start
  in
  let stop = if one.stop.offset >= other.stop.offset then one.stop else other.stop in
  { one with start; stop }

let spans_one_line region = region.start.line = region.stop.line

let to_string region =
  Printf.sprintf "%s:%d:%d" region.file region.start.line region.start.column

let show region =
  Printf.sprintf "%s:%d:%d-%d:%d(%d..%d)" region.file region.start.line
    region.start.column region.stop.line region.stop.column region.start.offset
    region.stop.offset

let pp formatter region = Format.pp_print_string formatter (show region)
