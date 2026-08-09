type type_ctor = { id : Data.Name.t; data : Typedef.Impl.t list }
[@@deriving show]

type t = { name : Data.Name.t; ctors : type_ctor list; params : string list }
[@@deriving show]

let of_frontend (td : Frontend.Typedecl.t) : t =
  let ctors =
    List.map
      (fun (ctor : Frontend.Typedecl.type_ctor) ->
        { id = Data.Name.local ctor.id; data = List.map Typedef.Impl.of_frontend ctor.data })
      td.ctors
  in
  { name = Data.Name.local td.name; ctors; params = td.params }
