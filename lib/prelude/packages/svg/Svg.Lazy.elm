{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/svg -- https://github.com/elm/svg
   Copyright (c) 2014-present, Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Svg.Lazy exposing
  ( lazy, lazy2, lazy3, lazy4, lazy5, lazy6, lazy7, lazy8
  )

import Basics exposing (..)
import Maybe exposing (Maybe(..))
import String
import Svg exposing (Svg)
import VirtualDom


lazy : (a -> Svg msg) -> a -> Svg msg
lazy =
  VirtualDom.lazy


lazy2 : (a -> b -> Svg msg) -> a -> b -> Svg msg
lazy2 =
  VirtualDom.lazy2


lazy3 : (a -> b -> c -> Svg msg) -> a -> b -> c -> Svg msg
lazy3 =
  VirtualDom.lazy3


lazy4 : (a -> b -> c -> d -> Svg msg) -> a -> b -> c -> d -> Svg msg
lazy4 =
  VirtualDom.lazy4


lazy5 : (a -> b -> c -> d -> e -> Svg msg) -> a -> b -> c -> d -> e -> Svg msg
lazy5 =
  VirtualDom.lazy5


lazy6 : (a -> b -> c -> d -> e -> f -> Svg msg) -> a -> b -> c -> d -> e -> f -> Svg msg
lazy6 =
  VirtualDom.lazy6


lazy7 : (a -> b -> c -> d -> e -> f -> g -> Svg msg) -> a -> b -> c -> d -> e -> f -> g -> Svg msg
lazy7 =
  VirtualDom.lazy7


lazy8 : (a -> b -> c -> d -> e -> f -> g -> h -> Svg msg) -> a -> b -> c -> d -> e -> f -> g -> h -> Svg msg
lazy8 =
  VirtualDom.lazy8
