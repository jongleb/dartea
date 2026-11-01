{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/svg -- https://github.com/elm/svg
   Copyright (c) 2014-present, Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Svg exposing
  ( Svg, Attribute, text, node, map
  , svg, foreignObject
  , circle, ellipse, image, line, path, polygon, polyline, rect, use
  , animate, animateColor, animateMotion, animateTransform, mpath, set
  , desc, metadata, title
  , a, defs, g, marker, mask, pattern, switch, symbol
  , altGlyph, altGlyphDef, altGlyphItem, glyph, glyphRef, textPath, text_
  , tref, tspan
  , font
  , linearGradient, radialGradient, stop
  , feBlend, feColorMatrix, feComponentTransfer, feComposite
  , feConvolveMatrix, feDiffuseLighting, feDisplacementMap, feFlood, feFuncA
  , feFuncB, feFuncG, feFuncR, feGaussianBlur, feImage, feMerge, feMergeNode
  , feMorphology, feOffset, feSpecularLighting, feTile, feTurbulence
  , feDistantLight, fePointLight, feSpotLight
  , clipPath, colorProfile, cursor, filter, style, view
  )

import Basics exposing (..)
import Maybe exposing (Maybe(..))
import String
import Html
import VirtualDom
import Json.Encode as Json


-- PRIMITIVES


type alias Svg msg =
  VirtualDom.Node msg


type alias Attribute msg =
  VirtualDom.Attribute msg


node : String -> List (Attribute msg) -> List (Svg msg) -> Svg msg
node =
  VirtualDom.nodeNS "http://www.w3.org/2000/svg"


text : String -> Svg msg
text =
  VirtualDom.text


map : (a -> msg) -> Svg a -> Svg msg
map =
  VirtualDom.map


-- HELPERS


trustedNode : String -> List (Attribute msg) -> List (Svg msg) -> Svg msg
trustedNode =
  Elm.Kernel.VirtualDom.nodeNS "http://www.w3.org/2000/svg"


-- TAGS


svg : List (Html.Attribute msg) -> List (Svg msg) -> Html.Html msg
svg =
  trustedNode "svg"


foreignObject : List (Attribute msg) -> List (Html.Html msg) -> Svg msg
foreignObject =
  trustedNode "foreignObject"


-- Animation elements


animate : List (Attribute msg) -> List (Svg msg) -> Svg msg
animate =
  trustedNode "animate"


animateColor : List (Attribute msg) -> List (Svg msg) -> Svg msg
animateColor =
  trustedNode "animateColor"


animateMotion : List (Attribute msg) -> List (Svg msg) -> Svg msg
animateMotion =
  trustedNode "animateMotion"


animateTransform : List (Attribute msg) -> List (Svg msg) -> Svg msg
animateTransform =
  trustedNode "animateTransform"


mpath : List (Attribute msg) -> List (Svg msg) -> Svg msg
mpath =
  trustedNode "mpath"


set : List (Attribute msg) -> List (Svg msg) -> Svg msg
set =
  trustedNode "set"


-- Container elements


a : List (Attribute msg) -> List (Svg msg) -> Svg msg
a =
  trustedNode "a"


defs : List (Attribute msg) -> List (Svg msg) -> Svg msg
defs =
  trustedNode "defs"


g : List (Attribute msg) -> List (Svg msg) -> Svg msg
g =
  trustedNode "g"


marker : List (Attribute msg) -> List (Svg msg) -> Svg msg
marker =
  trustedNode "marker"


mask : List (Attribute msg) -> List (Svg msg) -> Svg msg
mask =
  trustedNode "mask"


pattern : List (Attribute msg) -> List (Svg msg) -> Svg msg
pattern =
  trustedNode "pattern"


switch : List (Attribute msg) -> List (Svg msg) -> Svg msg
switch =
  trustedNode "switch"


symbol : List (Attribute msg) -> List (Svg msg) -> Svg msg
symbol =
  trustedNode "symbol"


-- Descriptive elements


desc : List (Attribute msg) -> List (Svg msg) -> Svg msg
desc =
  trustedNode "desc"


metadata : List (Attribute msg) -> List (Svg msg) -> Svg msg
metadata =
  trustedNode "metadata"


title : List (Attribute msg) -> List (Svg msg) -> Svg msg
title =
  trustedNode "title"


-- Filter primitive elements


feBlend : List (Attribute msg) -> List (Svg msg) -> Svg msg
feBlend =
  trustedNode "feBlend"


feColorMatrix : List (Attribute msg) -> List (Svg msg) -> Svg msg
feColorMatrix =
  trustedNode "feColorMatrix"


feComponentTransfer : List (Attribute msg) -> List (Svg msg) -> Svg msg
feComponentTransfer =
  trustedNode "feComponentTransfer"


feComposite : List (Attribute msg) -> List (Svg msg) -> Svg msg
feComposite =
  trustedNode "feComposite"


feConvolveMatrix : List (Attribute msg) -> List (Svg msg) -> Svg msg
feConvolveMatrix =
  trustedNode "feConvolveMatrix"


feDiffuseLighting : List (Attribute msg) -> List (Svg msg) -> Svg msg
feDiffuseLighting =
  trustedNode "feDiffuseLighting"


feDisplacementMap : List (Attribute msg) -> List (Svg msg) -> Svg msg
feDisplacementMap =
  trustedNode "feDisplacementMap"


feFlood : List (Attribute msg) -> List (Svg msg) -> Svg msg
feFlood =
  trustedNode "feFlood"


feFuncA : List (Attribute msg) -> List (Svg msg) -> Svg msg
feFuncA =
  trustedNode "feFuncA"


feFuncB : List (Attribute msg) -> List (Svg msg) -> Svg msg
feFuncB =
  trustedNode "feFuncB"


feFuncG : List (Attribute msg) -> List (Svg msg) -> Svg msg
feFuncG =
  trustedNode "feFuncG"


feFuncR : List (Attribute msg) -> List (Svg msg) -> Svg msg
feFuncR =
  trustedNode "feFuncR"


feGaussianBlur : List (Attribute msg) -> List (Svg msg) -> Svg msg
feGaussianBlur =
  trustedNode "feGaussianBlur"


feImage : List (Attribute msg) -> List (Svg msg) -> Svg msg
feImage =
  trustedNode "feImage"


feMerge : List (Attribute msg) -> List (Svg msg) -> Svg msg
feMerge =
  trustedNode "feMerge"


feMergeNode : List (Attribute msg) -> List (Svg msg) -> Svg msg
feMergeNode =
  trustedNode "feMergeNode"


feMorphology : List (Attribute msg) -> List (Svg msg) -> Svg msg
feMorphology =
  trustedNode "feMorphology"


feOffset : List (Attribute msg) -> List (Svg msg) -> Svg msg
feOffset =
  trustedNode "feOffset"


feSpecularLighting : List (Attribute msg) -> List (Svg msg) -> Svg msg
feSpecularLighting =
  trustedNode "feSpecularLighting"


feTile : List (Attribute msg) -> List (Svg msg) -> Svg msg
feTile =
  trustedNode "feTile"


feTurbulence : List (Attribute msg) -> List (Svg msg) -> Svg msg
feTurbulence =
  trustedNode "feTurbulence"


-- Font elements


font : List (Attribute msg) -> List (Svg msg) -> Svg msg
font =
  trustedNode "font"


-- Gradient elements


linearGradient : List (Attribute msg) -> List (Svg msg) -> Svg msg
linearGradient =
  trustedNode "linearGradient"


radialGradient : List (Attribute msg) -> List (Svg msg) -> Svg msg
radialGradient =
  trustedNode "radialGradient"


stop : List (Attribute msg) -> List (Svg msg) -> Svg msg
stop =
  trustedNode "stop"


-- Graphics elements


circle : List (Attribute msg) -> List (Svg msg) -> Svg msg
circle =
  trustedNode "circle"


ellipse : List (Attribute msg) -> List (Svg msg) -> Svg msg
ellipse =
  trustedNode "ellipse"


image : List (Attribute msg) -> List (Svg msg) -> Svg msg
image =
  trustedNode "image"


line : List (Attribute msg) -> List (Svg msg) -> Svg msg
line =
  trustedNode "line"


path : List (Attribute msg) -> List (Svg msg) -> Svg msg
path =
  trustedNode "path"


polygon : List (Attribute msg) -> List (Svg msg) -> Svg msg
polygon =
  trustedNode "polygon"


polyline : List (Attribute msg) -> List (Svg msg) -> Svg msg
polyline =
  trustedNode "polyline"


rect : List (Attribute msg) -> List (Svg msg) -> Svg msg
rect =
  trustedNode "rect"


use : List (Attribute msg) -> List (Svg msg) -> Svg msg
use =
  trustedNode "use"


-- Light source elements


feDistantLight : List (Attribute msg) -> List (Svg msg) -> Svg msg
feDistantLight =
  trustedNode "feDistantLight"


fePointLight : List (Attribute msg) -> List (Svg msg) -> Svg msg
fePointLight =
  trustedNode "fePointLight"


feSpotLight : List (Attribute msg) -> List (Svg msg) -> Svg msg
feSpotLight =
  trustedNode "feSpotLight"


-- Text content elements


altGlyph : List (Attribute msg) -> List (Svg msg) -> Svg msg
altGlyph =
  trustedNode "altGlyph"


altGlyphDef : List (Attribute msg) -> List (Svg msg) -> Svg msg
altGlyphDef =
  trustedNode "altGlyphDef"


altGlyphItem : List (Attribute msg) -> List (Svg msg) -> Svg msg
altGlyphItem =
  trustedNode "altGlyphItem"


glyph : List (Attribute msg) -> List (Svg msg) -> Svg msg
glyph =
  trustedNode "glyph"


glyphRef : List (Attribute msg) -> List (Svg msg) -> Svg msg
glyphRef =
  trustedNode "glyphRef"


textPath : List (Attribute msg) -> List (Svg msg) -> Svg msg
textPath =
  trustedNode "textPath"


text_ : List (Attribute msg) -> List (Svg msg) -> Svg msg
text_ =
  trustedNode "text"


tref : List (Attribute msg) -> List (Svg msg) -> Svg msg
tref =
  trustedNode "tref"


tspan : List (Attribute msg) -> List (Svg msg) -> Svg msg
tspan =
  trustedNode "tspan"


-- Uncategorized elements


clipPath : List (Attribute msg) -> List (Svg msg) -> Svg msg
clipPath =
  trustedNode "clipPath"


colorProfile : List (Attribute msg) -> List (Svg msg) -> Svg msg
colorProfile =
  trustedNode "colorProfile"


cursor : List (Attribute msg) -> List (Svg msg) -> Svg msg
cursor =
  trustedNode "cursor"


filter : List (Attribute msg) -> List (Svg msg) -> Svg msg
filter =
  trustedNode "filter"


style : List (Attribute msg) -> List (Svg msg) -> Svg msg
style =
  trustedNode "style"


view : List (Attribute msg) -> List (Svg msg) -> Svg msg
view =
  trustedNode "view"
