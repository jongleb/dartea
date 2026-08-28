{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/html -- https://github.com/elm/html
   Copyright (c) 2014-present, Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Html.Attributes exposing
    ( autofocus, checked, class, classList, for, hidden, href, id, map, name
    , placeholder, src, style, title, type_, value
    )

import Basics exposing (..)
import Json.Encode
import List
import String
import Tuple
import VirtualDom


stringProperty : String -> String -> VirtualDom.Attribute msg
stringProperty key given =
    VirtualDom.property key (Json.Encode.string given)


boolProperty : String -> Bool -> VirtualDom.Attribute msg
boolProperty key given =
    VirtualDom.property key (Json.Encode.bool given)


class : String -> VirtualDom.Attribute msg
class =
    stringProperty "className"


classList : List ( String, Bool ) -> VirtualDom.Attribute msg
classList pairs =
    class
        (String.join " "
            (List.map Tuple.first (List.filter Tuple.second pairs))
        )


id : String -> VirtualDom.Attribute msg
id =
    stringProperty "id"


title : String -> VirtualDom.Attribute msg
title =
    stringProperty "title"


hidden : Bool -> VirtualDom.Attribute msg
hidden =
    boolProperty "hidden"


type_ : String -> VirtualDom.Attribute msg
type_ =
    stringProperty "type"


value : String -> VirtualDom.Attribute msg
value =
    stringProperty "value"


checked : Bool -> VirtualDom.Attribute msg
checked =
    boolProperty "checked"


placeholder : String -> VirtualDom.Attribute msg
placeholder =
    stringProperty "placeholder"


autofocus : Bool -> VirtualDom.Attribute msg
autofocus =
    boolProperty "autofocus"


name : String -> VirtualDom.Attribute msg
name =
    stringProperty "name"


for : String -> VirtualDom.Attribute msg
for =
    stringProperty "htmlFor"


href : String -> VirtualDom.Attribute msg
href =
    stringProperty "href"


src : String -> VirtualDom.Attribute msg
src =
    stringProperty "src"


style : String -> String -> VirtualDom.Attribute msg
style =
    VirtualDom.style


map : (a -> msg) -> VirtualDom.Attribute a -> VirtualDom.Attribute msg
map =
    VirtualDom.mapAttribute
