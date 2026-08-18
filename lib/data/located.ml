type 'a t = { region : Region.t; thing : 'a } [@@deriving show]

let at region thing = { region; thing }
let mk thing lexing_positions = at (Region.of_lexing lexing_positions) thing
let dummy thing = at Region.nowhere thing
let ( ~? ) = dummy
let region { region; _ } = region
let line { region; _ } = region.Region.start.line
let unwrap { thing; _ } = thing
let map f { region; thing } = { region; thing = f thing }
