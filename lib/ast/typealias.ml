type t = {
  typedef : Typedef.t;
  params : string list;
  name : string Data.Located.t;
}
[@@deriving show, make]
