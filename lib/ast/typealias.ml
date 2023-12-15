type t = {
  typedef : Typedef.Impl.t;
  params : string Data.Located.t list;
  name : string Data.Located.t;
}
[@@deriving show, fields]
