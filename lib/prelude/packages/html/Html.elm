{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/html -- https://github.com/elm/html
   Copyright (c) 2014-present, Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Html exposing
    ( Attribute, Html
    , a, button, div, footer, form, h1, header, input, label, li, node, p
    , section, span, strong, text, ul
    )

import VirtualDom


type alias Html msg =
    VirtualDom.Node msg


type alias Attribute msg =
    VirtualDom.Attribute msg


node : String -> List (Attribute msg) -> List (Html msg) -> Html msg
node =
    VirtualDom.node


text : String -> Html msg
text =
    VirtualDom.text


a : List (Attribute msg) -> List (Html msg) -> Html msg
a =
    node "a"


button : List (Attribute msg) -> List (Html msg) -> Html msg
button =
    node "button"


div : List (Attribute msg) -> List (Html msg) -> Html msg
div =
    node "div"


footer : List (Attribute msg) -> List (Html msg) -> Html msg
footer =
    node "footer"


form : List (Attribute msg) -> List (Html msg) -> Html msg
form =
    node "form"


header : List (Attribute msg) -> List (Html msg) -> Html msg
header =
    node "header"


label : List (Attribute msg) -> List (Html msg) -> Html msg
label =
    node "label"


section : List (Attribute msg) -> List (Html msg) -> Html msg
section =
    node "section"


strong : List (Attribute msg) -> List (Html msg) -> Html msg
strong =
    node "strong"


h1 : List (Attribute msg) -> List (Html msg) -> Html msg
h1 =
    node "h1"


input : List (Attribute msg) -> List (Html msg) -> Html msg
input =
    node "input"


li : List (Attribute msg) -> List (Html msg) -> Html msg
li =
    node "li"


p : List (Attribute msg) -> List (Html msg) -> Html msg
p =
    node "p"


span : List (Attribute msg) -> List (Html msg) -> Html msg
span =
    node "span"


ul : List (Attribute msg) -> List (Html msg) -> Html msg
ul =
    node "ul"
