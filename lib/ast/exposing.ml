type privacy = Public of Data.Located.loc | Private [@@deriving show]

type upper = { name : string Data.Located.t; privacy : privacy }
[@@deriving show]

type exposed = Lower of string Data.Located.t | Upper of upper
[@@deriving show]

type t = Open | Explicit of exposed list [@@deriving show]
