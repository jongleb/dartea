type t =
  | Basics
  | Browser
  | Char
  | Dict
  | Html
  | Html_attributes
  | Html_events
  | Html_keyed
  | Html_lazy
  | Json_decode
  | Json_encode
  | List
  | Maybe
  | Result
  | String
  | Tuple
  | VirtualDom
[@@deriving enumerate]

let name = function
  | Basics -> "Basics"
  | Browser -> "Browser"
  | Char -> "Char"
  | Dict -> "Dict"
  | Html -> "Html"
  | Html_attributes -> "Html.Attributes"
  | Html_events -> "Html.Events"
  | Html_keyed -> "Html.Keyed"
  | Html_lazy -> "Html.Lazy"
  | Json_decode -> "Json.Decode"
  | Json_encode -> "Json.Encode"
  | List -> "List"
  | Maybe -> "Maybe"
  | Result -> "Result"
  | String -> "String"
  | Tuple -> "Tuple"
  | VirtualDom -> "VirtualDom"

let compiled_by =
  [
    "Compiled by dartea, an independent compiler. Not affiliated with or";
    "endorsed by the Elm project.";
  ]

let derived_from source copyright =
  [
    "Contains material derived from " ^ source ^ ",";
    copyright ^ ", under the BSD 3-Clause License.";
    "dartea's LICENSE carries the full text.";
  ]

let notice module_ =
  compiled_by
  @
  match module_ with
  | VirtualDom ->
      derived_from "elm/virtual-dom" "Copyright (c) 2016-present Evan Czaplicki"
  | Html | Html_attributes | Html_events | Html_keyed | Html_lazy ->
      derived_from "elm/html" "Copyright (c) 2014-present Evan Czaplicki"
  | Browser ->
      derived_from "elm/browser" "Copyright 2017-present Evan Czaplicki"
  | Json_decode | Json_encode ->
      derived_from "elm/json" "Copyright 2014-present Evan Czaplicki"
  | Basics | Char | Dict | List | Maybe | Result | String | Tuple ->
      derived_from "elm/core" "Copyright 2014-present Evan Czaplicki"

let source = function
  | Basics -> Prelude_source.basics
  | Browser -> Prelude_source.browser
  | Char -> Prelude_source.char
  | Dict -> Prelude_source.dict
  | Html -> Prelude_source.html
  | Html_attributes -> Prelude_source.html_attributes
  | Html_events -> Prelude_source.html_events
  | Html_keyed -> Prelude_source.html_keyed
  | Html_lazy -> Prelude_source.html_lazy
  | Json_decode -> Prelude_source.json_decode
  | Json_encode -> Prelude_source.json_encode
  | List -> Prelude_source.list
  | Maybe -> Prelude_source.maybe
  | Result -> Prelude_source.result
  | String -> Prelude_source.string
  | Tuple -> Prelude_source.tuple
  | VirtualDom -> Prelude_source.virtual_dom

let imported_by_default = function
  | Basics | Char | List | Maybe | Result | String | Tuple -> true
  | Browser | Dict | Html | Html_attributes | Html_events | Html_keyed
  | Html_lazy | Json_decode | Json_encode | VirtualDom ->
      false

let exposed_by_default = function
  | Basics -> Canonical.Exposed.All
  | Maybe ->
      Canonical.Exposed.Only
        [ Canonical.Exposed.Type { name = name Maybe; ctors_exposed = true } ]
  | Result ->
      Canonical.Exposed.Only
        [ Canonical.Exposed.Type { name = name Result; ctors_exposed = true } ]
  | Browser | Char | Dict | Html | Html_attributes | Html_events | Html_keyed
  | Html_lazy | Json_decode | Json_encode | List | String | Tuple | VirtualDom ->
      Canonical.Exposed.Only []

let default_imports : Canonical.Import.t list =
  List.map
    (fun module_ : Canonical.Import.t ->
      {
        module_name = name module_;
        alias = None;
        exposed = exposed_by_default module_;
        region = Data.Region.nowhere;
      })
    (List.filter imported_by_default all)
