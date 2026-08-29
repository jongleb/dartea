{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/core -- https://github.com/elm/core
   Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}module Platform.Cmd exposing (Cmd, batch, none)

import List


type Effect msg
    = Effect


type Cmd msg
    = None
    | Batch (List (Cmd msg))
    | Perform (Effect msg)


none : Cmd msg
none =
    None


batch : List (Cmd msg) -> Cmd msg
batch =
    Batch
