type t = {
  module_name : string;
  alias : string option;
  exposed : Exposed.t;
  region : Data.Region.t;
}
[@@deriving show]

let of_frontend (import : Frontend.Import_thing.t) =
  {
    module_name = Data.Located.unwrap import.Frontend.Import_thing.name;
    alias = import.alias;
    exposed = Exposed.of_frontend import.exposing;
    region = import.Frontend.Import_thing.name.region;
  }
