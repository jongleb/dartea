{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/html -- https://github.com/elm/html
   Copyright (c) 2014-present, Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Html.Attributes exposing
    ( class, href, id, placeholder, src, style, title, type_, value )

import VirtualDom


class : String -> VirtualDom.Attribute msg
class =
    VirtualDom.attribute "class"


href : String -> VirtualDom.Attribute msg
href =
    VirtualDom.attribute "href"


id : String -> VirtualDom.Attribute msg
id =
    VirtualDom.attribute "id"


placeholder : String -> VirtualDom.Attribute msg
placeholder =
    VirtualDom.attribute "placeholder"


src : String -> VirtualDom.Attribute msg
src =
    VirtualDom.attribute "src"


style : String -> String -> VirtualDom.Attribute msg
style =
    VirtualDom.style


title : String -> VirtualDom.Attribute msg
title =
    VirtualDom.attribute "title"


type_ : String -> VirtualDom.Attribute msg
type_ =
    VirtualDom.attribute "type"


value : String -> VirtualDom.Attribute msg
value =
    VirtualDom.attribute "value"
