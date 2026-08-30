{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/browser -- https://github.com/elm/browser
   Copyright 2017-present Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Browser.Events exposing
    ( Visibility(..), onAnimationFrame, onAnimationFrameDelta, onClick, onKeyDown
    , onKeyPress, onKeyUp, onMouseDown, onMouseMove, onMouseUp, onResize
    , onVisibilityChange
    )

import Basics exposing (..)

import Json.Decode as Decode
import Platform.Sub exposing (Sub)
import Time


onAnimationFrame : (Time.Posix -> msg) -> Sub msg
onAnimationFrame toMsg =
    Elm.Kernel.Browser.onAnimationFrame (\millis -> toMsg (Time.millisToPosix millis))


onAnimationFrameDelta : (Float -> msg) -> Sub msg
onAnimationFrameDelta =
    Elm.Kernel.Browser.onAnimationFrameDelta


onKeyPress : Decode.Decoder msg -> Sub msg
onKeyPress =
    on Document "keypress"


onKeyDown : Decode.Decoder msg -> Sub msg
onKeyDown =
    on Document "keydown"


onKeyUp : Decode.Decoder msg -> Sub msg
onKeyUp =
    on Document "keyup"


onClick : Decode.Decoder msg -> Sub msg
onClick =
    on Document "click"


onMouseMove : Decode.Decoder msg -> Sub msg
onMouseMove =
    on Document "mousemove"


onMouseDown : Decode.Decoder msg -> Sub msg
onMouseDown =
    on Document "mousedown"


onMouseUp : Decode.Decoder msg -> Sub msg
onMouseUp =
    on Document "mouseup"


onResize : (Int -> Int -> msg) -> Sub msg
onResize func =
    on Window "resize" <|
        Decode.field "target" <|
            Decode.map2 func
                (Decode.field "innerWidth" Decode.int)
                (Decode.field "innerHeight" Decode.int)


onVisibilityChange : (Visibility -> msg) -> Sub msg
onVisibilityChange func =
    on Document "visibilitychange" <|
        Decode.map (withHidden func) <|
            Decode.field "target" <|
                Decode.field "hidden" Decode.bool


withHidden : (Visibility -> msg) -> Bool -> msg
withHidden func isHidden =
    func
        (if isHidden then
            Hidden

         else
            Visible
        )


type Visibility
    = Visible
    | Hidden


type Node
    = Document
    | Window


on : Node -> String -> Decode.Decoder msg -> Sub msg
on node name decoder =
    listen (nodeName node) name decoder


nodeName : Node -> String
nodeName node =
    case node of
        Document ->
            "document"

        Window ->
            "window"


listen : String -> String -> Decode.Decoder msg -> Sub msg
listen =
    Elm.Kernel.Browser.on
