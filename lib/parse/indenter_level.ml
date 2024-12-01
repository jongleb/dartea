type kind = Indent | Dedent [@@deriving show]
type state = kind option ref [@@deriving show]
type indent_token = kind option ref [@@deriving show]

let state : state = ref None
