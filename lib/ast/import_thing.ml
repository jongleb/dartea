type t = {
  name : string Data.Located.t;
  alias : string option;
  exposing : Exposing.t;
}
[@@deriving show]
