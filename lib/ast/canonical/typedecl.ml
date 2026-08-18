type type_ctor = {
  id : Data.Name.t;
  data : Typedef.Impl.t list;
  region : Data.Region.t;
}
[@@deriving show]

type t = {
  name : Data.Name.t;
  ctors : type_ctor list;
  params : string list;
  region : Data.Region.t;
}
[@@deriving show]

let of_frontend (td : Frontend.Typedecl.t) : t =
  let ctors =
    List.map
      (fun (ctor : Frontend.Typedecl.type_ctor) ->
        {
          id = Data.Name.local ctor.id.thing;
          data = List.map Typedef.Impl.of_frontend ctor.data;
          region = ctor.id.region;
        })
      td.ctors
  in
  { name = Data.Name.local td.name.thing; ctors; params = td.params;
    region = td.name.region }
