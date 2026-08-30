{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/url -- https://github.com/elm/url
   Copyright 2017-present Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Url.Parser.Internal exposing
  ( QueryParser(..)
  )

import Basics exposing (..)
import List

import Dict

type QueryParser a =
  Parser (Dict.Dict String (List String) -> a)
