module Str_set = Set.Make (String)

type t = { constrs : Str_set.t }
