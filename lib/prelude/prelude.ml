type t =
  | Basics [@rename "Basics"]
  | Browser [@rename "Browser"]
  | Browser_dom [@rename "Browser.Dom"]
  | Char [@rename "Char"]
  | Dict [@rename "Dict"]
  | Html [@rename "Html"]
  | Html_attributes [@rename "Html.Attributes"]
  | Html_events [@rename "Html.Events"]
  | Html_keyed [@rename "Html.Keyed"]
  | Html_lazy [@rename "Html.Lazy"]
  | Json_decode [@rename "Json.Decode"]
  | Json_encode [@rename "Json.Encode"]
  | List [@rename "List"]
  | Maybe [@rename "Maybe"]
  | Platform [@rename "Platform"]
  | Platform_cmd [@rename "Platform.Cmd"]
  | Platform_sub [@rename "Platform.Sub"]
  | Result [@rename "Result"]
  | String [@rename "String"]
  | Task [@rename "Task"]
  | Time [@rename "Time"]
  | Tuple [@rename "Tuple"]
  | VirtualDom [@rename "VirtualDom"]
[@@deriving enumerate, to_string]

let name = to_string

let packages =
  [
    "elm/core"; "elm/json"; "elm/html"; "elm/browser"; "elm/time";
    "elm/virtual-dom";
  ]

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
  | Browser | Browser_dom ->
      derived_from "elm/browser" "Copyright 2017-present Evan Czaplicki"
  | Json_decode | Json_encode ->
      derived_from "elm/json" "Copyright 2014-present Evan Czaplicki"
  | Time ->
      derived_from "elm/time" "Copyright 2018-present Evan Czaplicki"
  | Basics | Char | Dict | List | Maybe | Platform | Platform_cmd
  | Platform_sub | Result | String | Task | Tuple ->
      derived_from "elm/core" "Copyright 2014-present Evan Czaplicki"

let source = function
  | Basics -> Prelude_source.basics
  | Browser -> Prelude_source.browser
  | Browser_dom -> Prelude_source.browser_dom
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
  | Platform -> Prelude_source.platform
  | Platform_cmd -> Prelude_source.platform_cmd
  | Platform_sub -> Prelude_source.platform_sub
  | Result -> Prelude_source.result
  | String -> Prelude_source.string
  | Task -> Prelude_source.task
  | Time -> Prelude_source.time
  | Tuple -> Prelude_source.tuple
  | VirtualDom -> Prelude_source.virtual_dom

let imported_by_default = function
  | Basics | Char | List | Maybe | Platform | Platform_cmd | Platform_sub | Result
  | String | Tuple ->
      true
  | Browser | Browser_dom | Dict | Html | Html_attributes | Html_events
  | Html_keyed | Html_lazy | Json_decode | Json_encode | Task | Time | VirtualDom ->
      false

let exposed_by_default = function
  | Basics -> Canonical.Exposed.All
  | Maybe ->
      Canonical.Exposed.Only
        [ Canonical.Exposed.Type { name = name Maybe; ctors_exposed = true } ]
  | Result ->
      Canonical.Exposed.Only
        [ Canonical.Exposed.Type { name = name Result; ctors_exposed = true } ]
  | Platform ->
      Canonical.Exposed.Only
        [ Canonical.Exposed.Type { name = "Program"; ctors_exposed = false } ]
  | Platform_cmd ->
      Canonical.Exposed.Only
        [ Canonical.Exposed.Type { name = "Cmd"; ctors_exposed = false } ]
  | Platform_sub ->
      Canonical.Exposed.Only
        [ Canonical.Exposed.Type { name = "Sub"; ctors_exposed = false } ]
  | Browser | Browser_dom | Char | Dict | Html | Html_attributes | Html_events
  | Html_keyed | Html_lazy | Json_decode | Json_encode | List | String | Task
  | Time | Tuple | VirtualDom ->
      Canonical.Exposed.Only []

let alias_by_default module_ =
  match module_ with
  | Platform_cmd -> Some "Cmd"
  | Platform_sub -> Some "Sub"
  | Basics | Browser | Browser_dom | Char | Dict | Html | Html_attributes
  | Html_events | Html_keyed | Html_lazy | Json_decode | Json_encode | List
  | Maybe | Platform | Result | String | Task | Time | Tuple | VirtualDom ->
      None

let default_imports : Canonical.Import.t list =
  List.map
    (fun module_ : Canonical.Import.t ->
      {
        module_name = name module_;
        alias = alias_by_default module_;
        exposed = exposed_by_default module_;
        region = Data.Region.nowhere;
      })
    (List.filter imported_by_default all)
