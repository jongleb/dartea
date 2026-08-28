{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/virtual-dom -- https://github.com/elm/virtual-dom
   Copyright (c) 2016-present, Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module VirtualDom exposing
    ( Attribute, Handler(..), Node, attribute, node, on, property, style, text )

import Json.Decode
import Json.Encode


type Node msg
    = Node


type Attribute msg
    = Attribute


node : String -> List (Attribute msg) -> List (Node msg) -> Node msg
node =
    Elm.Kernel.VirtualDom.node


text : String -> Node msg
text =
    Elm.Kernel.VirtualDom.text


attribute : String -> String -> Attribute msg
attribute =
    Elm.Kernel.VirtualDom.attribute


property : String -> Json.Encode.Value -> Attribute msg
property =
    Elm.Kernel.VirtualDom.property


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
