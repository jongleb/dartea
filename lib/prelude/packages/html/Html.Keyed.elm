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
