type ctor = { id : Data.Name.t; payload : Type.t list } [@@deriving show]

type t = { name : Data.Name.t; params : string list; ctors : ctor list }
[@@deriving show]

let constructors (decl : t) ~arguments =
  if List.length decl.params <> List.length arguments then None
  else
    let bindings =
      List.combine decl.params arguments |> Type.By_variable.of_list
    in
    Some
      (List.map
         (fun ctor ->
           { ctor with payload = List.map (Type.substitute bindings) ctor.payload })
         decl.ctors)
