open Lexing

type loc = position * position

let show_position position =
  Format.asprintf
    "{pos_fname = %s; npos_lnum = %d; npos_bol = %d; npos_cnum = %d;}"
    (if position.pos_fname = "" then {|""|} else position.pos_fname)
    position.pos_lnum position.pos_cnum position.pos_cnum

let pp_position fmt position = Format.fprintf position fmt

let show_loc (a, b) =
  Printf.sprintf "(%s,%s)" (show_position a) (show_position b)

let pp_loc fmt ab = Format.pp_print_string fmt @@ show_loc ab

let range_string loc =
  let pos_start, pos_end = loc in
  let line_start = pos_start.pos_lnum in
  let line_end = pos_end.pos_lnum in
  let col_start = pos_start.pos_cnum - pos_start.pos_bol in
  let col_end = pos_end.pos_cnum - pos_end.pos_bol in
  match (line_start, col_start, line_end, col_end) with
  | 0, -1, 0, -1 -> "(?, ?)-(?, ?)"
  | ls, cs, le, ce -> Printf.sprintf "(%d,%d)-(%d,%d)" ls cs le ce

type 'a t = { loc : loc; thing : 'a } [@@deriving show]

let mk thing loc = { thing; loc }
let dummy value = mk value (dummy_pos, dummy_pos)
let ( ~? ) = dummy
let line { loc = pos_start, _; _ } = pos_start.pos_lnum
