{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/url -- https://github.com/elm/url
   Copyright 2017-present Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Url.Builder exposing
  ( absolute, relative, crossOrigin, custom, Root(..)
  , QueryParameter, string, int, toQuery
  )

import Basics exposing (..)
import List
import Maybe exposing (Maybe(..))
import String

import Url

absolute : List String -> List QueryParameter -> String
absolute pathSegments parameters =
  "/" ++ String.join "/" pathSegments ++ toQuery parameters

relative : List String -> List QueryParameter -> String
relative pathSegments parameters =
  String.join "/" pathSegments ++ toQuery parameters

crossOrigin : String -> List String -> List QueryParameter -> String
crossOrigin prePath pathSegments parameters =
  prePath ++ "/" ++ String.join "/" pathSegments ++ toQuery parameters

type Root = Absolute | Relative | CrossOrigin String

custom : Root -> List String -> List QueryParameter -> Maybe String -> String
custom root pathSegments parameters maybeFragment =
  let
    fragmentless =
      rootToPrePath root ++ String.join "/" pathSegments ++ toQuery parameters
  in
  case maybeFragment of
    Nothing ->
      fragmentless

    Just fragment ->
      fragmentless ++ "#" ++ fragment

rootToPrePath : Root -> String
rootToPrePath root =
  case root of
    Absolute ->
      "/"

    Relative ->
      ""

    CrossOrigin prePath ->
      prePath ++ "/"

type QueryParameter =
  QueryParameter String String

string : String -> String -> QueryParameter
string key value =
  QueryParameter (Url.percentEncode key) (Url.percentEncode value)

int : String -> Int -> QueryParameter
int key value =
  QueryParameter (Url.percentEncode key) (String.fromInt value)

toQuery : List QueryParameter -> String
toQuery parameters =
  case parameters of
    [] ->
      ""

    _ ->
      "?" ++ String.join "&" (List.map toQueryPair parameters)

toQueryPair : QueryParameter -> String
toQueryPair (QueryParameter key value) =
  key ++ "=" ++ value
