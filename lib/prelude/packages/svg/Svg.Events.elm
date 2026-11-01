{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/svg -- https://github.com/elm/svg
   Copyright (c) 2014-present, Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Svg.Events exposing
  ( onClick, onMouseDown, onMouseUp, onMouseOver, onMouseOut
  , on, stopPropagationOn, preventDefaultOn, custom
  )

import Basics exposing (..)
import Maybe exposing (Maybe(..))
import String
import Html.Events as Html
import Json.Decode as Json
import Svg exposing (Attribute)
import VirtualDom


-- MOUSE


onClick : msg -> Attribute msg
onClick msg =
  Html.on "click" (Json.succeed msg)


onMouseDown : msg -> Attribute msg
onMouseDown msg =
  Html.on "mousedown" (Json.succeed msg)


onMouseUp : msg -> Attribute msg
onMouseUp msg =
  Html.on "mouseup" (Json.succeed msg)


onMouseOver : msg -> Attribute msg
onMouseOver msg =
  Html.on "mouseover" (Json.succeed msg)


onMouseOut : msg -> Attribute msg
onMouseOut msg =
  Html.on "mouseout" (Json.succeed msg)


-- CUSTOM


on : String -> Json.Decoder msg -> Attribute msg
on =
  Html.on


stopPropagationOn : String -> Json.Decoder (msg, Bool) -> Attribute msg
stopPropagationOn =
  Html.stopPropagationOn


preventDefaultOn : String -> Json.Decoder (msg, Bool) -> Attribute msg
preventDefaultOn =
  Html.preventDefaultOn


custom : String -> Json.Decoder { message : msg, stopPropagation : Bool, preventDefault : Bool } -> Attribute msg
custom =
  Html.custom
