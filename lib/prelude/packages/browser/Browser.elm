{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/browser -- https://github.com/elm/browser
   Copyright 2017-present Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Browser exposing (Program, sandbox)

import VirtualDom


type Program flags model msg
    = Program


type alias Sandbox model msg =
    { init : model
    , update : msg -> model -> model
    , view : model -> VirtualDom.Node msg
    }


sandbox : Sandbox model msg -> Program () model msg
sandbox =
    Elm.Kernel.Browser.sandbox
