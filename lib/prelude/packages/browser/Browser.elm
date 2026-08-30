{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/browser -- https://github.com/elm/browser
   Copyright 2017-present Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Browser exposing (Document, UrlRequest(..), application, document, element, sandbox)

import Basics exposing (..)
import List
import Maybe exposing (Maybe(..))
import String

import Browser.Navigation as Navigation
import Platform exposing (Program)
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)
import Url
import VirtualDom


type alias Sandbox model msg =
    { init : model
    , update : msg -> model -> model
    , view : model -> VirtualDom.Node msg
    }


sandbox : Sandbox model msg -> Program () model msg
sandbox =
    Elm.Kernel.Browser.sandbox


element :
    { init : flags -> ( model, Cmd msg )
    , view : model -> VirtualDom.Node msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    }
    -> Program flags model msg
element =
    Elm.Kernel.Browser.element


type alias Document msg =
    { title : String
    , body : List (VirtualDom.Node msg)
    }


document :
    { init : flags -> ( model, Cmd msg )
    , view : model -> Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    }
    -> Program flags model msg
document =
    Elm.Kernel.Browser.document


type alias Application flags model msg =
    { init : flags -> Url.Url -> Navigation.Key -> ( model, Cmd msg )
    , view : model -> Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , onUrlRequest : UrlRequest -> msg
    , onUrlChange : Url.Url -> msg
    }


application : Application flags model msg -> Program flags model msg
application impl =
    applicationWith Url.fromString impl


applicationWith :
    (String -> Maybe Url.Url)
    -> Application flags model msg
    -> Program flags model msg
applicationWith =
    Elm.Kernel.Browser.application


type UrlRequest
    = Internal Url.Url
    | External String
