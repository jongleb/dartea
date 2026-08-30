{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/url -- https://github.com/elm/url
   Copyright 2017-present Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Url exposing
  ( Url
  , Protocol(..)
  , toString
  , fromString
  , percentEncode
  , percentDecode
  )

import Basics exposing (..)
import Maybe exposing (Maybe(..))
import String


type alias Url =
  { protocol : Protocol
  , host : String
  , port_ : Maybe Int
  , path : String
  , query : Maybe String
  , fragment : Maybe String
  }

type Protocol = Http | Https

fromString : String -> Maybe Url
fromString str =
  if String.startsWith "http://" str then
    chompAfterProtocol Http (String.dropLeft 7 str)

  else if String.startsWith "https://" str then
    chompAfterProtocol Https (String.dropLeft 8 str)

  else
    Nothing

chompAfterProtocol : Protocol -> String -> Maybe Url
chompAfterProtocol protocol str =
  if String.isEmpty str then
    Nothing
  else
    case String.indexes "#" str of
      [] ->
        chompBeforeFragment protocol Nothing str

      i :: _ ->
        chompBeforeFragment protocol (Just (String.dropLeft (i + 1) str)) (String.left i str)

chompBeforeFragment : Protocol -> Maybe String -> String -> Maybe Url
chompBeforeFragment protocol frag str =
  if String.isEmpty str then
    Nothing
  else
    case String.indexes "?" str of
      [] ->
        chompBeforeQuery protocol Nothing frag str

      i :: _ ->
        chompBeforeQuery protocol (Just (String.dropLeft (i + 1) str)) frag (String.left i str)

chompBeforeQuery : Protocol -> Maybe String -> Maybe String -> String -> Maybe Url
chompBeforeQuery protocol params frag str =
  if String.isEmpty str then
    Nothing
  else
    case String.indexes "/" str of
      [] ->
        chompBeforePath protocol "/" params frag str

      i :: _ ->
        chompBeforePath protocol (String.dropLeft i str) params frag (String.left i str)

chompBeforePath : Protocol -> String -> Maybe String -> Maybe String -> String -> Maybe Url
chompBeforePath protocol path params frag str =
  if String.isEmpty str || String.contains "@" str then
    Nothing
  else
    case String.indexes ":" str of
      [] ->
        Just <| Url protocol str Nothing path params frag

      i :: [] ->
        case String.toInt (String.dropLeft (i + 1) str) of
          Nothing ->
            Nothing

          port_ ->
            Just <| Url protocol (String.left i str) port_ path params frag

      _ ->
        Nothing

toString : Url -> String
toString url =
  let
    http =
      case url.protocol of
        Http ->
          "http://"

        Https ->
          "https://"
  in
  addPort url.port_ (http ++ url.host) ++ url.path
    |> addPrefixed "?" url.query
    |> addPrefixed "#" url.fragment

addPort : Maybe Int -> String -> String
addPort maybePort starter =
  case maybePort of
    Nothing ->
      starter

    Just port_ ->
      starter ++ ":" ++ String.fromInt port_

addPrefixed : String -> Maybe String -> String -> String
addPrefixed prefix maybeSegment starter =
  case maybeSegment of
    Nothing ->
      starter

    Just segment ->
      starter ++ prefix ++ segment

percentEncode : String -> String
percentEncode =
  Elm.Kernel.Url.percentEncode

percentDecode : String -> Maybe String
percentDecode =
  Elm.Kernel.Url.percentDecode
