type t = Basics | Char | List | Maybe | Result | String | Tuple
[@@deriving enumerate, variants]

let name = Variants.to_name

let notice =
  [
    "Derived from elm/core -- https://github.com/elm/core";
    "Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.";
    "Emitted by dartea; its LICENSE file carries the full text.";
  ]

let source = function
  | Basics -> Prelude_source.basics
  | Char -> Prelude_source.char
  | List -> Prelude_source.list
  | Maybe -> Prelude_source.maybe
  | Result -> Prelude_source.result
  | String -> Prelude_source.string
  | Tuple -> Prelude_source.tuple

let exposed_by_default = function
  | Basics -> Canonical.Exposed.All
  | Maybe ->
      Canonical.Exposed.Only
        [ Canonical.Exposed.Type { name = name Maybe; ctors_exposed = true } ]
  | Result ->
      Canonical.Exposed.Only
        [ Canonical.Exposed.Type { name = name Result; ctors_exposed = true } ]
  | Char | List | String | Tuple -> Canonical.Exposed.Only []

let default_imports : Canonical.Import.t list =
  List.map
    (fun module_ : Canonical.Import.t ->
      {
        module_name = name module_;
        alias = None;
        exposed = exposed_by_default module_;
        region = Data.Region.nowhere;
      })
    all
