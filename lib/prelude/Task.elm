{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/core -- https://github.com/elm/core
   Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}module Task exposing (Task, attempt)

import Platform.Cmd exposing (Cmd)
import Result exposing (Result)


type Task x a
    = Task


attempt : (Result x a -> msg) -> Task x a -> Cmd msg
attempt =
    Elm.Kernel.Task.attempt
