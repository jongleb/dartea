module J = Ast

module Node = struct
  type t =
    | First_child [@rename "firstChild"]
    | Next_sibling [@rename "nextSibling"]
  [@@deriving to_string]
end

let member held node = J.member held (Node.to_string node)
