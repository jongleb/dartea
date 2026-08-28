{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/html -- https://github.com/elm/html
   Copyright (c) 2014-present, Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Html.Lazy exposing
    ( lazy, lazy2, lazy3, lazy4, lazy5, lazy6, lazy7, lazy8 )

import Html
import VirtualDom


lazy : (a -> Html.Html msg) -> a -> Html.Html msg
lazy =
    VirtualDom.lazy


lazy2 : (a -> b -> Html.Html msg) -> a -> b -> Html.Html msg
lazy2 =
    VirtualDom.lazy2


lazy3 : (a -> b -> c -> Html.Html msg) -> a -> b -> c -> Html.Html msg
lazy3 =
    VirtualDom.lazy3


lazy4 : (a -> b -> c -> d -> Html.Html msg) -> a -> b -> c -> d -> Html.Html msg
lazy4 =
    VirtualDom.lazy4


lazy5 :
    (a -> b -> c -> d -> e -> Html.Html msg)
    -> a
    -> b
    -> c
    -> d
    -> e
    -> Html.Html msg
lazy5 =
    VirtualDom.lazy5


lazy6 :
    (a -> b -> c -> d -> e -> f -> Html.Html msg)
    -> a
    -> b
    -> c
    -> d
    -> e
    -> f
    -> Html.Html msg
lazy6 =
    VirtualDom.lazy6


lazy7 :
    (a -> b -> c -> d -> e -> f -> g -> Html.Html msg)
    -> a
    -> b
    -> c
    -> d
    -> e
    -> f
    -> g
    -> Html.Html msg
lazy7 =
    VirtualDom.lazy7


lazy8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> Html.Html msg)
    -> a
    -> b
    -> c
    -> d
    -> e
    -> f
    -> g
    -> h
    -> Html.Html msg
lazy8 =
    VirtualDom.lazy8
