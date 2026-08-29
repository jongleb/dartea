{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/html -- https://github.com/elm/html
   Copyright (c) 2014-present, Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Html.Attributes exposing
    ( accept, acceptCharset, accesskey, action, align, alt, attribute
    , autocomplete, autofocus, autoplay, checked, cite, class, classList
    , cols, colspan, contenteditable, contextmenu, controls, coords, datetime
    , default, dir, disabled, download, draggable, dropzone, enctype, for
    , form, headers, height, hidden, href, hreflang, id, ismap, itemprop
    , kind, lang, list, loop, manifest, map, max, maxlength, media, method
    , min, minlength, multiple, name, novalidate, pattern, ping, placeholder
    , poster, preload, property, pubdate, readonly, rel, required, reversed
    , rows, rowspan, sandbox, scope, selected, shape, size, spellcheck, src
    , srcdoc, srclang, start, step, style, tabindex, target, title, type_
    , usemap, value, width, wrap
    )

import Basics exposing (..)
import Html exposing (Attribute)
import Json.Encode as Json
import List
import String
import Tuple
import VirtualDom


style : String -> String -> Attribute msg
style =
  VirtualDom.style


classList : List (String, Bool) -> Attribute msg
classList classes =
  class <| String.join " " <| List.map Tuple.first <|
    List.filter Tuple.second classes


property : String -> Json.Value -> Attribute msg
property =
  VirtualDom.property


stringProperty : String -> String -> Attribute msg
stringProperty key string =
  VirtualDom.property key (Json.string string)


boolProperty : String -> Bool -> Attribute msg
boolProperty key bool =
  VirtualDom.property key (Json.bool bool)


attribute : String -> String -> Attribute msg
attribute =
  VirtualDom.attribute


map : (a -> msg) -> Attribute a -> Attribute msg
map =
  VirtualDom.mapAttribute


class : String -> Attribute msg
class =
  stringProperty "className"


hidden : Bool -> Attribute msg
hidden =
  boolProperty "hidden"


id : String -> Attribute msg
id =
  stringProperty "id"


title : String -> Attribute msg
title =
  stringProperty "title"


accesskey : Char -> Attribute msg
accesskey char =
  stringProperty "accessKey" (String.fromChar char)


contenteditable : Bool -> Attribute msg
contenteditable =
  boolProperty "contentEditable"


contextmenu : String -> Attribute msg
contextmenu =
  VirtualDom.attribute "contextmenu"


dir : String -> Attribute msg
dir =
  stringProperty "dir"


draggable : String -> Attribute msg
draggable =
  VirtualDom.attribute "draggable"


dropzone : String -> Attribute msg
dropzone =
  stringProperty "dropzone"


itemprop : String -> Attribute msg
itemprop =
  VirtualDom.attribute "itemprop"


lang : String -> Attribute msg
lang =
  stringProperty "lang"


spellcheck : Bool -> Attribute msg
spellcheck =
  boolProperty "spellcheck"


tabindex : Int -> Attribute msg
tabindex n =
  VirtualDom.attribute "tabIndex" (String.fromInt n)


src : String -> Attribute msg
src url =
  stringProperty "src" url


height : Int -> Attribute msg
height n =
  VirtualDom.attribute "height" (String.fromInt n)


width : Int -> Attribute msg
width n =
  VirtualDom.attribute "width" (String.fromInt n)


alt : String -> Attribute msg
alt =
  stringProperty "alt"


autoplay : Bool -> Attribute msg
autoplay =
  boolProperty "autoplay"


controls : Bool -> Attribute msg
controls =
  boolProperty "controls"


loop : Bool -> Attribute msg
loop =
  boolProperty "loop"


preload : String -> Attribute msg
preload =
  stringProperty "preload"


poster : String -> Attribute msg
poster =
  stringProperty "poster"


default : Bool -> Attribute msg
default =
  boolProperty "default"


kind : String -> Attribute msg
kind =
  stringProperty "kind"


label : String -> Attribute msg
label =
  stringProperty "label"


srclang : String -> Attribute msg
srclang =
  stringProperty "srclang"


sandbox : String -> Attribute msg
sandbox =
  stringProperty "sandbox"


srcdoc : String -> Attribute msg
srcdoc =
  stringProperty "srcdoc"


type_ : String -> Attribute msg
type_ =
  stringProperty "type"


value : String -> Attribute msg
value =
  stringProperty "value"


checked : Bool -> Attribute msg
checked =
  boolProperty "checked"


placeholder : String -> Attribute msg
placeholder =
  stringProperty "placeholder"


selected : Bool -> Attribute msg
selected =
  boolProperty "selected"


accept : String -> Attribute msg
accept =
  stringProperty "accept"


acceptCharset : String -> Attribute msg
acceptCharset =
  stringProperty "acceptCharset"


action : String -> Attribute msg
action uri =
  stringProperty "action" uri


autocomplete : Bool -> Attribute msg
autocomplete bool =
  stringProperty "autocomplete" (if bool then "on" else "off")


autofocus : Bool -> Attribute msg
autofocus =
  boolProperty "autofocus"


disabled : Bool -> Attribute msg
disabled =
  boolProperty "disabled"


enctype : String -> Attribute msg
enctype =
  stringProperty "enctype"


list : String -> Attribute msg
list =
  VirtualDom.attribute "list"


minlength : Int -> Attribute msg
minlength n =
  VirtualDom.attribute "minLength" (String.fromInt n)


maxlength : Int -> Attribute msg
maxlength n =
  VirtualDom.attribute "maxlength" (String.fromInt n)


method : String -> Attribute msg
method =
  stringProperty "method"


multiple : Bool -> Attribute msg
multiple =
  boolProperty "multiple"


name : String -> Attribute msg
name =
  stringProperty "name"


novalidate : Bool -> Attribute msg
novalidate =
  boolProperty "noValidate"


pattern : String -> Attribute msg
pattern =
  stringProperty "pattern"


readonly : Bool -> Attribute msg
readonly =
  boolProperty "readOnly"


required : Bool -> Attribute msg
required =
  boolProperty "required"


size : Int -> Attribute msg
size n =
  VirtualDom.attribute "size" (String.fromInt n)


for : String -> Attribute msg
for =
  stringProperty "htmlFor"


form : String -> Attribute msg
form =
  VirtualDom.attribute "form"


max : String -> Attribute msg
max =
  stringProperty "max"


min : String -> Attribute msg
min =
  stringProperty "min"


step : String -> Attribute msg
step n =
  stringProperty "step" n


cols : Int -> Attribute msg
cols n =
  VirtualDom.attribute "cols" (String.fromInt n)


rows : Int -> Attribute msg
rows n =
  VirtualDom.attribute "rows" (String.fromInt n)


wrap : String -> Attribute msg
wrap =
  stringProperty "wrap"


ismap : Bool -> Attribute msg
ismap =
  boolProperty "isMap"


usemap : String -> Attribute msg
usemap =
  stringProperty "useMap"


shape : String -> Attribute msg
shape =
  stringProperty "shape"


coords : String -> Attribute msg
coords =
  stringProperty "coords"


align : String -> Attribute msg
align =
  stringProperty "align"


cite : String -> Attribute msg
cite =
  stringProperty "cite"


href : String -> Attribute msg
href url =
  stringProperty "href" url


target : String -> Attribute msg
target =
  stringProperty "target"


download : String -> Attribute msg
download fileName =
  stringProperty "download" fileName


downloadAs : String -> Attribute msg
downloadAs =
  stringProperty "download"


hreflang : String -> Attribute msg
hreflang =
  stringProperty "hreflang"


media : String -> Attribute msg
media =
  VirtualDom.attribute "media"


ping : String -> Attribute msg
ping =
  stringProperty "ping"


rel : String -> Attribute msg
rel =
  VirtualDom.attribute "rel"


datetime : String -> Attribute msg
datetime =
  VirtualDom.attribute "datetime"


pubdate : String -> Attribute msg
pubdate =
  VirtualDom.attribute "pubdate"


reversed : Bool -> Attribute msg
reversed =
  boolProperty "reversed"


start : Int -> Attribute msg
start n =
  stringProperty "start" (String.fromInt n)


colspan : Int -> Attribute msg
colspan n =
  VirtualDom.attribute "colspan" (String.fromInt n)


headers : String -> Attribute msg
headers =
  stringProperty "headers"


rowspan : Int -> Attribute msg
rowspan n =
  VirtualDom.attribute "rowspan" (String.fromInt n)


scope : String -> Attribute msg
scope =
  stringProperty "scope"


manifest : String -> Attribute msg
manifest =
  VirtualDom.attribute "manifest"


span : Int -> Attribute msg
span n =
    stringProperty "span" (String.fromInt n)
