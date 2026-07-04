type t = { params : string list; typedef : Typedef.Impl.t; name : string }
[@@deriving show]

let of_frontend (frontend : Frontend.Typealias.t) : t =
  {
    name = frontend.name.thing;
    params = List.map (fun p -> p.Data.Located.thing) frontend.params;
    typedef = Typedef.Impl.of_frontend frontend.typedef;
  }
