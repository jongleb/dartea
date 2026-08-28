{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/core -- https://github.com/elm/core
   Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Char exposing
    ( isUpper, isLower, isAlpha, isAlphaNum
    , isDigit, isOctDigit, isHexDigit
    , toUpper, toLower
    , toCode, fromCode
    )


isUpper : Char -> Bool
isUpper char =
    let
        code =
            toCode char
    in
    code <= 0x5A && 0x41 <= code


isLower : Char -> Bool
isLower char =
    let
        code =
            toCode char
    in
    0x61 <= code && code <= 0x7A


isAlpha : Char -> Bool
isAlpha char =
    isLower char || isUpper char


isAlphaNum : Char -> Bool
isAlphaNum char =
    isLower char || isUpper char || isDigit char


isDigit : Char -> Bool
isDigit char =
    let
        code =
            toCode char
    in
    code <= 0x39 && 0x30 <= code


isOctDigit : Char -> Bool
isOctDigit char =
    let
        code =
            toCode char
    in
    code <= 0x37 && 0x30 <= code


isHexDigit : Char -> Bool
isHexDigit char =
    let
        code =
            toCode char
    in
    (0x30 <= code && code <= 0x39)
        || (0x41 <= code && code <= 0x46)
        || (0x61 <= code && code <= 0x66)


toUpper : Char -> Char
toUpper =
    Elm.Kernel.Char.toUpper


toLower : Char -> Char
toLower =
    Elm.Kernel.Char.toLower


toCode : Char -> Int
toCode =
    Elm.Kernel.Char.toCode


fromCode : Int -> Char
fromCode =
    Elm.Kernel.Char.fromCode
