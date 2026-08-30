{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/http -- https://github.com/elm/http
   Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Http exposing
    ( Body, Error(..), Expect, Header, Metadata, Part, Progress(..), Resolver
    , Response(..), cancel, emptyBody, expectJson, expectString
    , expectStringResponse, expectWhatever, fractionReceived, fractionSent, get
    , header, jsonBody, multipartBody, post, request, riskyRequest, riskyTask
    , stringBody, stringPart, stringResolver, task, track
    )

import Basics exposing (..)
import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import List
import Maybe exposing (Maybe(..))
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)
import Result exposing (Result(..))
import String
import Task exposing (Task)


type alias Options msg =
    { method : String
    , headers : List Header
    , url : String
    , body : Body
    , expect : Expect msg
    , timeout : Maybe Float
    , tracker : Maybe String
    }


get : { url : String, expect : Expect msg } -> Cmd msg
get r =
    request
        { method = "GET"
        , headers = []
        , url = r.url
        , body = emptyBody
        , expect = r.expect
        , timeout = Nothing
        , tracker = Nothing
        }


post : { url : String, body : Body, expect : Expect msg } -> Cmd msg
post r =
    request
        { method = "POST"
        , headers = []
        , url = r.url
        , body = r.body
        , expect = r.expect
        , timeout = Nothing
        , tracker = Nothing
        }


request : Options msg -> Cmd msg
request r =
    send (prepared False r)


riskyRequest : Options msg -> Cmd msg
riskyRequest r =
    send (prepared True r)


type alias Prepared expect =
    { method : String
    , headers : List Header
    , url : String
    , body : Body
    , expect : expect
    , timeout : Maybe Float
    , tracker : Maybe String
    , allowCookiesFromOtherDomains : Bool
    }


prepared : Bool -> Options msg -> Prepared (Expect msg)
prepared risky r =
    { method = r.method
    , headers = r.headers
    , url = r.url
    , body = r.body
    , expect = r.expect
    , timeout = r.timeout
    , tracker = r.tracker
    , allowCookiesFromOtherDomains = risky
    }


send : Prepared (Expect msg) -> Cmd msg
send =
    Elm.Kernel.Http.request


type Header
    = Header String String


header : String -> String -> Header
header =
    Header


type Body
    = Body


emptyBody : Body
emptyBody =
    Elm.Kernel.Http.emptyBody


jsonBody : Encode.Value -> Body
jsonBody value =
    stringBody "application/json" (Encode.encode 0 value)


stringBody : String -> String -> Body
stringBody =
    Elm.Kernel.Http.pair


multipartBody : List Part -> Body
multipartBody parts =
    stringBody "" (formData parts)


formData : List Part -> String
formData =
    Elm.Kernel.Http.toFormData


type Part
    = Part


stringPart : String -> String -> Part
stringPart =
    Elm.Kernel.Http.pair


type Expect msg
    = Expect


expectString : (Result Error String -> msg) -> Expect msg
expectString toMsg =
    expectStringResponse toMsg (resolve Ok)


expectJson : (Result Error a -> msg) -> Decode.Decoder a -> Expect msg
expectJson toMsg decoder =
    expectStringResponse toMsg <|
        resolve <|
            \string ->
                Result.mapError Decode.errorToString (Decode.decodeString decoder string)


expectWhatever : (Result Error () -> msg) -> Expect msg
expectWhatever toMsg =
    expectStringResponse toMsg (resolve (\_ -> Ok ()))


resolve : (body -> Result String a) -> Response body -> Result Error a
resolve toResult response =
    case response of
        BadUrl_ url ->
            Err (BadUrl url)

        Timeout_ ->
            Err Timeout

        NetworkError_ ->
            Err NetworkError

        BadStatus_ metadata _ ->
            Err (BadStatus metadata.statusCode)

        GoodStatus_ _ body ->
            Result.mapError BadBody (toResult body)


type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int
    | BadBody String


expectStringResponse : (Result x a -> msg) -> (Response String -> Result x a) -> Expect msg
expectStringResponse toMsg toResult =
    expectWith (responseOf >> toResult >> toMsg)


expectWith : (Raw -> msg) -> Expect msg
expectWith =
    Elm.Kernel.Http.expect


type alias Raw =
    { kind : String
    , url : String
    , statusCode : Int
    , statusText : String
    , headers : List ( String, String )
    , body : String
    }


responseOf : Raw -> Response String
responseOf raw =
    let
        metadata =
            { url = raw.url
            , statusCode = raw.statusCode
            , statusText = raw.statusText
            , headers = Dict.fromList raw.headers
            }
    in
    case raw.kind of
        "badUrl" ->
            BadUrl_ raw.url

        "timeout" ->
            Timeout_

        "network" ->
            NetworkError_

        "bad" ->
            BadStatus_ metadata raw.body

        _ ->
            GoodStatus_ metadata raw.body


type Response body
    = BadUrl_ String
    | Timeout_
    | NetworkError_
    | BadStatus_ Metadata body
    | GoodStatus_ Metadata body


type alias Metadata =
    { url : String
    , statusCode : Int
    , statusText : String
    , headers : Dict String String
    }


cancel : String -> Cmd msg
cancel =
    Elm.Kernel.Http.cancel


track : String -> (Progress -> msg) -> Sub msg
track =
    Elm.Kernel.Http.track


type Progress
    = Sending { sent : Int, size : Int }
    | Receiving { received : Int, size : Maybe Int }


fractionSent : { sent : Int, size : Int } -> Float
fractionSent p =
    if p.size == 0 then
        1

    else
        clamp 0 1 (toFloat p.sent / toFloat p.size)


fractionReceived : { received : Int, size : Maybe Int } -> Float
fractionReceived p =
    case p.size of
        Nothing ->
            0

        Just n ->
            if n == 0 then
                1

            else
                clamp 0 1 (toFloat p.received / toFloat n)


type alias TaskOptions x a =
    { method : String
    , headers : List Header
    , url : String
    , body : Body
    , resolver : Resolver x a
    , timeout : Maybe Float
    }


task : TaskOptions x a -> Task x a
task r =
    resolved (preparedTask False r)


riskyTask : TaskOptions x a -> Task x a
riskyTask r =
    resolved (preparedTask True r)


preparedTask : Bool -> TaskOptions x a -> Prepared (Resolver x a)
preparedTask risky r =
    { method = r.method
    , headers = r.headers
    , url = r.url
    , body = r.body
    , expect = r.resolver
    , timeout = r.timeout
    , tracker = Nothing
    , allowCookiesFromOtherDomains = risky
    }


resolved : Prepared (Resolver x a) -> Task x a
resolved =
    Elm.Kernel.Http.toTask


type Resolver x a
    = Resolver


stringResolver : (Response String -> Result x a) -> Resolver x a
stringResolver toResult =
    resolverWith (responseOf >> toResult)


resolverWith : (Raw -> Result x a) -> Resolver x a
resolverWith =
    Elm.Kernel.Http.expect
