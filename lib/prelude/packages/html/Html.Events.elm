{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/html -- https://github.com/elm/html
   Copyright (c) 2014-present, Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Html.Events exposing
    ( custom, keyCode, on, onBlur, onCheck, onClick, onDoubleClick, onFocus
    , onInput, onMouseDown, onMouseEnter, onMouseLeave, onMouseOut, onMouseOver
    , onMouseUp, onSubmit, preventDefaultOn, stopPropagationOn, targetChecked
    , targetValue
    )

import Basics exposing (..)
import Json.Decode
import VirtualDom


on : String -> Json.Decode.Decoder msg -> VirtualDom.Attribute msg
on event decoder =
    VirtualDom.on event (VirtualDom.Normal decoder)


stopPropagationOn :
    String -> Json.Decode.Decoder ( msg, Bool ) -> VirtualDom.Attribute msg
stopPropagationOn event decoder =
    VirtualDom.on event (VirtualDom.MayStopPropagation decoder)


preventDefaultOn :
    String -> Json.Decode.Decoder ( msg, Bool ) -> VirtualDom.Attribute msg
preventDefaultOn event decoder =
    VirtualDom.on event (VirtualDom.MayPreventDefault decoder)


custom :
    String
    ->
        Json.Decode.Decoder
            { message : msg, stopPropagation : Bool, preventDefault : Bool }
    -> VirtualDom.Attribute msg
custom event decoder =
    VirtualDom.on event (VirtualDom.Custom decoder)


simple : String -> msg -> VirtualDom.Attribute msg
simple event given =
    on event (Json.Decode.succeed given)


onClick : msg -> VirtualDom.Attribute msg
onClick =
    simple "click"


onDoubleClick : msg -> VirtualDom.Attribute msg
onDoubleClick =
    simple "dblclick"


onMouseDown : msg -> VirtualDom.Attribute msg
onMouseDown =
    simple "mousedown"


onMouseUp : msg -> VirtualDom.Attribute msg
onMouseUp =
    simple "mouseup"


onMouseEnter : msg -> VirtualDom.Attribute msg
onMouseEnter =
    simple "mouseenter"


onMouseLeave : msg -> VirtualDom.Attribute msg
onMouseLeave =
    simple "mouseleave"


onMouseOver : msg -> VirtualDom.Attribute msg
onMouseOver =
    simple "mouseover"


onMouseOut : msg -> VirtualDom.Attribute msg
onMouseOut =
    simple "mouseout"


onBlur : msg -> VirtualDom.Attribute msg
onBlur =
    simple "blur"


onFocus : msg -> VirtualDom.Attribute msg
onFocus =
    simple "focus"


targetValue : Json.Decode.Decoder String
targetValue =
    Json.Decode.at [ "target", "value" ] Json.Decode.string


targetChecked : Json.Decode.Decoder Bool
targetChecked =
    Json.Decode.at [ "target", "checked" ] Json.Decode.bool


keyCode : Json.Decode.Decoder Int
keyCode =
    Json.Decode.field "keyCode" Json.Decode.int


alwaysStop : a -> ( a, Bool )
alwaysStop given =
    ( given, True )


onInput : (String -> msg) -> VirtualDom.Attribute msg
onInput tagger =
    stopPropagationOn "input"
        (Json.Decode.map alwaysStop (Json.Decode.map tagger targetValue))


onCheck : (Bool -> msg) -> VirtualDom.Attribute msg
onCheck tagger =
    on "change" (Json.Decode.map tagger targetChecked)


onSubmit : msg -> VirtualDom.Attribute msg
onSubmit given =
    preventDefaultOn "submit"
        (Json.Decode.map alwaysStop (Json.Decode.succeed given))
