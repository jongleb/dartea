{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/browser -- https://github.com/elm/browser
   Copyright 2017-present Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}module Browser.Dom exposing (Error(..), focus)

import Task exposing (Task)


type Error
    = NotFound String


focus : String -> Task Error ()
focus =
    Elm.Kernel.Dom.focus
