{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/html -- https://github.com/elm/html
   Copyright (c) 2014-present, Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Html.Keyed exposing (node, ol, ul)

import Html
import VirtualDom


node :
    String
    -> List (Html.Attribute msg)
    -> List ( String, Html.Html msg )
    -> Html.Html msg
node =
    VirtualDom.keyedNode


ol :
    List (Html.Attribute msg)
    -> List ( String, Html.Html msg )
    -> Html.Html msg
ol =
    node "ol"


ul :
    List (Html.Attribute msg)
    -> List ( String, Html.Html msg )
    -> Html.Html msg
ul =
    node "ul"
