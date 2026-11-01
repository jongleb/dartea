{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/svg -- https://github.com/elm/svg
   Copyright (c) 2014-present, Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Svg.Keyed exposing ( node )

import Basics exposing (..)
import Maybe exposing (Maybe(..))
import String
import Svg exposing (Attribute, Svg)
import VirtualDom
import Json.Encode as Json


node : String -> List (Attribute msg) -> List ( String, Svg msg ) -> Svg msg
node =
  VirtualDom.keyedNodeNS "http://www.w3.org/2000/svg"
