{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/browser -- https://github.com/elm/browser
   Copyright 2017-present Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Browser.Navigation exposing (Key, back, forward, load, pushUrl, reload, reloadAndSkipCache, replaceUrl)

import Basics exposing (..)

import Platform.Cmd exposing (Cmd)


type Key
    = Key


pushUrl : Key -> String -> Cmd msg
pushUrl =
    Elm.Kernel.Browser.pushUrl


replaceUrl : Key -> String -> Cmd msg
replaceUrl =
    Elm.Kernel.Browser.replaceUrl


back : Key -> Int -> Cmd msg
back key n =
    go key (negate n)


forward : Key -> Int -> Cmd msg
forward =
    go


go : Key -> Int -> Cmd msg
go =
    Elm.Kernel.Browser.go


load : String -> Cmd msg
load =
    Elm.Kernel.Browser.load


reload : Cmd msg
reload =
    reloadWith False


reloadAndSkipCache : Cmd msg
reloadAndSkipCache =
    reloadWith True


reloadWith : Bool -> Cmd msg
reloadWith =
    Elm.Kernel.Browser.reload
