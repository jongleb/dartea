type t = {
  params : string list;
  typedef : Typedef.Impl.t;
  name : Data.Name.t;
  region : Data.Region.t;
}
[@@deriving show]

let of_frontend (frontend : Frontend.Typealias.t) : t =
  {
    name = Data.Name.local frontend.name.thing;
    region = frontend.name.region;
    params = List.map (fun p -> p.Data.Located.thing) frontend.params;
    typedef = Typedef.Impl.of_frontend frontend.typedef;
  }
