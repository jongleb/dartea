type t = Basics | Maybe | String | Tuple

let all = [ Basics; Maybe; String; Tuple ]

let name = function
  | Basics -> "Basics"
  | Maybe -> "Maybe"
  | String -> "String"
  | Tuple -> "Tuple"

let source = function
  | Basics -> Prelude_source.basics
  | Maybe -> Prelude_source.maybe
  | String -> Prelude_source.string
  | Tuple -> Prelude_source.tuple

let exposed_by_default = function
  | Basics -> Canonical.Exposed.All
  | Maybe ->
      Canonical.Exposed.Only
        [ Canonical.Exposed.Type { name = name Maybe; ctors_exposed = true } ]
  | String | Tuple -> Canonical.Exposed.Only []

let default_imports : Canonical.Import.t list =
  List.map
    (fun module_ : Canonical.Import.t ->
      {
        module_name = name module_;
        alias = None;
        exposed = exposed_by_default module_;
      })
    all
