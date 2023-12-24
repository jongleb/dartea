module Map_aliases = Map.Make (String)

type t = { typealiases : Typealias.t Map_aliases.t }
