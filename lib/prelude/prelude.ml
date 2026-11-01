type t =
  | Basics [@rename "Basics"]
  | Browser [@rename "Browser"]
  | Browser_dom [@rename "Browser.Dom"]
  | Browser_events [@rename "Browser.Events"]
  | Browser_navigation [@rename "Browser.Navigation"]
  | Char [@rename "Char"]
  | Dict [@rename "Dict"]
  | Html [@rename "Html"]
  | Html_attributes [@rename "Html.Attributes"]
  | Html_events [@rename "Html.Events"]
  | Html_keyed [@rename "Html.Keyed"]
  | Html_lazy [@rename "Html.Lazy"]
  | Http [@rename "Http"]
  | Json_decode [@rename "Json.Decode"]
  | Json_encode [@rename "Json.Encode"]
  | List [@rename "List"]
  | Maybe [@rename "Maybe"]
  | Platform [@rename "Platform"]
  | Platform_cmd [@rename "Platform.Cmd"]
  | Platform_sub [@rename "Platform.Sub"]
  | Result [@rename "Result"]
  | String [@rename "String"]
  | Random [@rename "Random"]
  | Parser [@rename "Parser"]
  | Parser_advanced [@rename "Parser.Advanced"]
  | Set [@rename "Set"]
  | Bitwise [@rename "Bitwise"]
  | Svg [@rename "Svg"]
  | Svg_attributes [@rename "Svg.Attributes"]
  | Svg_events [@rename "Svg.Events"]
  | Svg_keyed [@rename "Svg.Keyed"]
  | Svg_lazy [@rename "Svg.Lazy"]
  | Task [@rename "Task"]
  | Time [@rename "Time"]
  | Tuple [@rename "Tuple"]
  | Url [@rename "Url"]
  | Url_builder [@rename "Url.Builder"]
  | Url_parser [@rename "Url.Parser"]
  | Url_parser_internal [@rename "Url.Parser.Internal"]
  | Url_parser_query [@rename "Url.Parser.Query"]
  | VirtualDom [@rename "VirtualDom"]
[@@deriving enumerate, to_string]

let name = to_string

let of_known (known : Primitives.Known_type.t) =
  match known with
  | Primitives.Known_type.Maybe -> Maybe
  | Primitives.Known_type.List -> List
  | Primitives.Known_type.Value -> Json_encode

let packages =
  [
    "elm/core"; "elm/json"; "elm/html"; "elm/browser"; "elm/time";
    "elm/virtual-dom"; "elm/url"; "elm/http"; "elm/svg"; "elm/parser"; "elm/random";
  ]

let derived_from source copyright =
  [
    "Contains material derived from " ^ source ^ ",";
    copyright ^ ", under the BSD 3-Clause License.";
    "dartea's LICENSE carries the full text.";
  ]

let derived module_ =
  match module_ with
  | Html_attributes -> derived_from "elm/html" "Copyright (c) 2014-present Evan Czaplicki"
  | Browser | Browser_events | Browser_navigation ->
      derived_from "elm/browser" "Copyright 2017-present Evan Czaplicki"
  | Url | Url_builder | Url_parser | Url_parser_internal | Url_parser_query ->
      derived_from "elm/url" "Copyright 2017-present Evan Czaplicki"
  | Http -> derived_from "elm/http" "Copyright 2014-present Evan Czaplicki"
  | Random -> derived_from "elm/random" "Copyright (c) 2018-present, Evan Czaplicki"
  | Parser | Parser_advanced ->
      derived_from "elm/parser" "Copyright (c) 2017-present, Evan Czaplicki"
  | Svg | Svg_attributes | Svg_events | Svg_keyed | Svg_lazy ->
      derived_from "elm/svg" "Copyright (c) 2014-present, Evan Czaplicki"
  | Basics | Bitwise | Char | Dict | List | Maybe | Result | Set | Tuple ->
      derived_from "elm/core" "Copyright 2014-present Evan Czaplicki"
  | VirtualDom | Html | Html_events | Html_keyed | Html_lazy | Browser_dom
  | Json_decode | Json_encode | Time | Platform | Platform_cmd | Platform_sub
  | String | Task ->
      []

let is_derived module_ = not (List.is_empty (derived module_))
let of_name written = List.find_opt (fun module_ -> String.equal (name module_) written) all

let source = function
  | Basics -> Prelude_source.basics
  | Browser -> Prelude_source.browser
  | Browser_dom -> Prelude_source.browser_dom
  | Browser_events -> Prelude_source.browser_events
  | Browser_navigation -> Prelude_source.browser_navigation
  | Char -> Prelude_source.char
  | Dict -> Prelude_source.dict
  | Html -> Prelude_source.html
  | Html_attributes -> Prelude_source.html_attributes
  | Html_events -> Prelude_source.html_events
  | Html_keyed -> Prelude_source.html_keyed
  | Html_lazy -> Prelude_source.html_lazy
  | Http -> Prelude_source.http
  | Json_decode -> Prelude_source.json_decode
  | Json_encode -> Prelude_source.json_encode
  | List -> Prelude_source.list
  | Maybe -> Prelude_source.maybe
  | Platform -> Prelude_source.platform
  | Platform_cmd -> Prelude_source.platform_cmd
  | Platform_sub -> Prelude_source.platform_sub
  | Result -> Prelude_source.result
  | String -> Prelude_source.string
  | Random -> Prelude_source.random
  | Parser -> Prelude_source.parser
  | Parser_advanced -> Prelude_source.parser_advanced
  | Set -> Prelude_source.set
  | Bitwise -> Prelude_source.bitwise
  | Svg -> Prelude_source.svg
  | Svg_attributes -> Prelude_source.svg_attributes
  | Svg_events -> Prelude_source.svg_events
  | Svg_keyed -> Prelude_source.svg_keyed
  | Svg_lazy -> Prelude_source.svg_lazy
  | Task -> Prelude_source.task
  | Time -> Prelude_source.time
  | Tuple -> Prelude_source.tuple
  | Url -> Prelude_source.url
  | Url_builder -> Prelude_source.url_builder
  | Url_parser -> Prelude_source.url_parser
  | Url_parser_internal -> Prelude_source.url_parser_internal
  | Url_parser_query -> Prelude_source.url_parser_query
  | VirtualDom -> Prelude_source.virtual_dom

let imported_by_default = function
  | Basics | Char | List | Maybe | Platform | Platform_cmd | Platform_sub | Result
  | String | Tuple ->
      true
  | Browser | Browser_dom | Browser_events | Browser_navigation | Dict | Html
  | Html_attributes | Html_events | Html_keyed | Html_lazy | Http | Json_decode
  | Json_encode | Task | Time | Url | Url_builder | Url_parser
  | Url_parser_internal | Url_parser_query | VirtualDom | Set | Bitwise | Parser | Parser_advanced | Random | Svg | Svg_attributes | Svg_events | Svg_keyed | Svg_lazy ->
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
  | Browser | Browser_dom | Browser_events | Browser_navigation | Char | Dict
  | Html | Html_attributes | Html_events | Html_keyed | Html_lazy | Http
  | Json_decode | Json_encode | List | String | Task | Time | Tuple | Url
  | Url_builder | Url_parser | Url_parser_internal | Url_parser_query | VirtualDom
  | Set | Bitwise | Parser | Parser_advanced | Random | Svg | Svg_attributes | Svg_events | Svg_keyed | Svg_lazy ->
      Canonical.Exposed.Only []

let alias_by_default module_ =
  match module_ with
  | Platform_cmd -> Some "Cmd"
  | Platform_sub -> Some "Sub"
  | Basics | Browser | Browser_dom | Browser_events | Browser_navigation | Char
  | Dict | Html | Html_attributes | Html_events | Html_keyed | Html_lazy | Http
  | Json_decode | Json_encode | List | Maybe | Platform | Result | String | Task
  | Time | Tuple | Url | Url_builder | Url_parser | Url_parser_internal
  | Url_parser_query | VirtualDom | Set | Bitwise | Parser | Parser_advanced | Random | Svg | Svg_attributes | Svg_events | Svg_keyed | Svg_lazy ->
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
