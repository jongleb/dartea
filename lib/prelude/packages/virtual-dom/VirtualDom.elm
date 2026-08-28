{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/virtual-dom -- https://github.com/elm/virtual-dom
   Copyright (c) 2016-present, Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module VirtualDom exposing
    ( Attribute, Handler(..), Node, attribute, attributeNS, keyedNode
    , keyedNodeNS, lazy, lazy2, lazy3, lazy4, lazy5, lazy6, lazy7, lazy8, map
    , mapAttribute, node, nodeNS, on, property, style, text
    )

import Basics exposing (..)
import Json.Decode
import Json.Encode
import Maybe
import Result exposing (Result(..))
import String


type Node msg
    = Node


type Attribute msg
    = Attribute


noScript : String -> String
noScript tag =
    if tag == "script" then
        "p"

    else
        tag


noOnOrFormAction : String -> String
noOnOrFormAction key =
    let
        lowered =
            String.toLower key
    in
    if String.startsWith "on" lowered || lowered == "formaction" then
        "data-" ++ key

    else
        key


noInnerHtmlOrFormAction : String -> String
noInnerHtmlOrFormAction key =
    if key == "innerHTML" || key == "formAction" then
        "data-" ++ key

    else
        key


dangerousUri : String -> Bool
dangerousUri value =
    let
        squeezed =
            String.toLower (String.filter notWhitespace value)

        trimmed =
            String.toLower (String.trimLeft value)
    in
    String.startsWith "javascript:" squeezed
        || String.startsWith "data:text/html" trimmed


notWhitespace : Char -> Bool
notWhitespace letter =
    not (String.isEmpty (String.trim (String.fromChar letter)))


noJavaScriptOrHtmlUri : String -> String
noJavaScriptOrHtmlUri value =
    if dangerousUri value then
        ""

    else
        value


noJavaScriptOrHtmlJson : Json.Encode.Value -> Json.Encode.Value
noJavaScriptOrHtmlJson value =
    case Json.Decode.decodeValue Json.Decode.string value of
        Ok written ->
            if dangerousUri written then
                Json.Encode.string ""

            else
                value

        Err _ ->
            value


node : String -> List (Attribute msg) -> List (Node msg) -> Node msg
node tag =
    Elm.Kernel.VirtualDom.node (noScript tag)


nodeNS :
    String
    -> String
    -> List (Attribute msg)
    -> List (Node msg)
    -> Node msg
nodeNS namespace tag =
    Elm.Kernel.VirtualDom.nodeNS namespace (noScript tag)


keyedNode :
    String
    -> List (Attribute msg)
    -> List ( String, Node msg )
    -> Node msg
keyedNode tag =
    Elm.Kernel.VirtualDom.keyedNode (noScript tag)


keyedNodeNS :
    String
    -> String
    -> List (Attribute msg)
    -> List ( String, Node msg )
    -> Node msg
keyedNodeNS namespace tag =
    Elm.Kernel.VirtualDom.keyedNodeNS namespace (noScript tag)


text : String -> Node msg
text =
    Elm.Kernel.VirtualDom.text


attribute : String -> String -> Attribute msg
attribute key value =
    Elm.Kernel.VirtualDom.attribute (noOnOrFormAction key)
        (noJavaScriptOrHtmlUri value)


attributeNS : String -> String -> String -> Attribute msg
attributeNS namespace key value =
    Elm.Kernel.VirtualDom.attributeNS namespace (noOnOrFormAction key)
        (noJavaScriptOrHtmlUri value)


property : String -> Json.Encode.Value -> Attribute msg
property key value =
    Elm.Kernel.VirtualDom.property (noInnerHtmlOrFormAction key)
        (noJavaScriptOrHtmlJson value)


style : String -> String -> Attribute msg
style =
    Elm.Kernel.VirtualDom.style


type Handler msg
    = Normal (Json.Decode.Decoder msg)
    | MayStopPropagation (Json.Decode.Decoder ( msg, Bool ))
    | MayPreventDefault (Json.Decode.Decoder ( msg, Bool ))
    | Custom
        (Json.Decode.Decoder
            { message : msg, stopPropagation : Bool, preventDefault : Bool }
        )


on : String -> Handler msg -> Attribute msg
on =
    Elm.Kernel.VirtualDom.on


map : (a -> msg) -> Node a -> Node msg
map =
    Elm.Kernel.VirtualDom.map


mapAttribute : (a -> b) -> Attribute a -> Attribute b
mapAttribute =
    Elm.Kernel.VirtualDom.mapAttribute


lazy : (a -> Node msg) -> a -> Node msg
lazy =
    Elm.Kernel.VirtualDom.lazy


lazy2 : (a -> b -> Node msg) -> a -> b -> Node msg
lazy2 =
    Elm.Kernel.VirtualDom.lazy2


lazy3 : (a -> b -> c -> Node msg) -> a -> b -> c -> Node msg
lazy3 =
    Elm.Kernel.VirtualDom.lazy3


lazy4 : (a -> b -> c -> d -> Node msg) -> a -> b -> c -> d -> Node msg
lazy4 =
    Elm.Kernel.VirtualDom.lazy4


lazy5 : (a -> b -> c -> d -> e -> Node msg) -> a -> b -> c -> d -> e -> Node msg
lazy5 =
    Elm.Kernel.VirtualDom.lazy5


lazy6 :
    (a -> b -> c -> d -> e -> f -> Node msg)
    -> a
    -> b
    -> c
    -> d
    -> e
    -> f
    -> Node msg
lazy6 =
    Elm.Kernel.VirtualDom.lazy6


lazy7 :
    (a -> b -> c -> d -> e -> f -> g -> Node msg)
    -> a
    -> b
    -> c
    -> d
    -> e
    -> f
    -> g
    -> Node msg
lazy7 =
    Elm.Kernel.VirtualDom.lazy7


lazy8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> Node msg)
    -> a
    -> b
    -> c
    -> d
    -> e
    -> f
    -> g
    -> h
    -> Node msg
lazy8 =
    Elm.Kernel.VirtualDom.lazy8
