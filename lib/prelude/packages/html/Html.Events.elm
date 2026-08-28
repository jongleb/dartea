{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/html -- https://github.com/elm/html
   Copyright (c) 2014-present, Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Html.Events exposing (on, onClick)

import VirtualDom


on : String -> msg -> VirtualDom.Attribute msg
on =
    VirtualDom.on


onClick : msg -> VirtualDom.Attribute msg
onClick =
    on "click"
