type type_ctor = { id : string; data : Typedef.Impl.t list } [@@deriving show]

type t = { name : string; ctors : type_ctor list; params : string list }
[@@deriving show]

let of_frontend (td : Frontend.Typedecl.t) : t =
  let ctors =
    List.map
      (fun (ctor : Frontend.Typedecl.type_ctor) ->
        { id = ctor.id; data = List.map Typedef.Impl.of_frontend ctor.data })
      td.ctors
  in
  { name = td.name; ctors; params = td.params }
