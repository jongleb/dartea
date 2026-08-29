{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/core -- https://github.com/elm/core
   Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module String exposing
    ( isEmpty, length, reverse, repeat, replace
    , append, concat, split, join, words, lines
    , slice, left, right, dropLeft, dropRight
    , contains, startsWith, endsWith, indexes, indices
    , toInt, fromInt
    , toFloat, fromFloat
    , fromChar, cons, uncons
    , toList, fromList
    , toUpper, toLower, pad, padLeft, padRight, trim, trimLeft, trimRight
    , map, filter, foldl, foldr, any, all
    )

import Basics exposing (..)
import Char
import List
import Maybe exposing (Maybe(..))


length : String -> Int
length =
    Elm.Kernel.String.length


append : String -> String -> String
append =
    Elm.Kernel.String.append


split : String -> String -> List String
split =
    Elm.Kernel.String.split


toList : String -> List Char
toList =
    Elm.Kernel.String.toList


fromList : List Char -> String
fromList =
    Elm.Kernel.String.fromList


takeLeft : Int -> String -> String
takeLeft =
    Elm.Kernel.String.takeLeft


dropLeftBy : Int -> String -> String
dropLeftBy =
    Elm.Kernel.String.dropLeft


isEmpty : String -> Bool
isEmpty text =
    text == ""


reverse : String -> String
reverse text =
    fromList (List.reverse (toList text))


repeat : Int -> String -> String
repeat n chunk =
    repeatHelp n chunk ""


repeatHelp : Int -> String -> String -> String
repeatHelp n chunk result =
    if n <= 0 then
        result

    else
        repeatHelp (n - 1) chunk (append result chunk)


concat : List String -> String
concat chunks =
    List.foldr append "" chunks


join : String -> List String -> String
join sep chunks =
    case chunks of
        [] ->
            ""

        first :: rest ->
            List.foldl (\chunk acc -> append (append acc sep) chunk) first rest


replace : String -> String -> String -> String
replace before after text =
    join after (split before text)


isSpace : Char -> Bool
isSpace char =
    let
        code =
            Char.toCode char
    in
    code == 32 || code == 10 || code == 13 || code == 9


words : String -> List String
words text =
    List.filter (\chunk -> chunk /= "") (List.foldr wordStep [ "" ] (toList text))


wordStep : Char -> List String -> List String
wordStep letter chunks =
    if isSpace letter then
        "" :: chunks

    else
        case chunks of
            current :: rest ->
                append (fromChar letter) current :: rest

            [] ->
                [ fromChar letter ]


lines : String -> List String
lines text =
    split "\n" (replace "\u{000D}\n" "\n" text)


clampIndex : Int -> Int -> Int
clampIndex index size =
    if index < 0 then
        max 0 (size + index)

    else
        min index size


slice : Int -> Int -> String -> String
slice start end text =
    let
        size =
            length text

        from =
            clampIndex start size

        to =
            clampIndex end size
    in
    if from >= to then
        ""

    else
        takeLeft (to - from) (dropLeftBy from text)


left : Int -> String -> String
left n text =
    if n < 1 then
        ""

    else
        takeLeft n text


right : Int -> String -> String
right n text =
    if n < 1 then
        ""

    else
        dropLeftBy (max 0 (length text - n)) text


dropLeft : Int -> String -> String
dropLeft n text =
    if n < 1 then
        text

    else
        dropLeftBy n text


dropRight : Int -> String -> String
dropRight n text =
    if n < 1 then
        text

    else
        takeLeft (max 0 (length text - n)) text


contains : String -> String -> Bool
contains needle text =
    if needle == "" then
        True

    else
        List.length (split needle text) > 1


startsWith : String -> String -> Bool
startsWith prefix text =
    left (length prefix) text == prefix


endsWith : String -> String -> Bool
endsWith suffix text =
    right (length suffix) text == suffix


indexes : String -> String -> List Int
indexes needle text =
    if needle == "" then
        []

    else
        case split needle text of
            [] ->
                []

            first :: rest ->
                indexesHelp (length needle) (length first) rest []


indexesHelp : Int -> Int -> List String -> List Int -> List Int
indexesHelp size position chunks found =
    case chunks of
        [] ->
            List.reverse found

        chunk :: rest ->
            indexesHelp size
                (position + size + length chunk)
                rest
                (position :: found)


indices : String -> String -> List Int
indices needle text =
    indexes needle text


isInt : String -> Bool
isInt =
    Elm.Kernel.String.isInt


toIntUnsafe : String -> Int
toIntUnsafe =
    Elm.Kernel.String.toIntUnsafe


toInt : String -> Maybe Int
toInt text =
    if isInt text then
        Just (toIntUnsafe text)

    else
        Nothing


fromInt : Int -> String
fromInt =
    Elm.Kernel.String.fromNumber


isFloat : String -> Bool
isFloat =
    Elm.Kernel.String.isFloat


toFloatUnsafe : String -> Float
toFloatUnsafe =
    Elm.Kernel.String.toFloatUnsafe


toFloat : String -> Maybe Float
toFloat text =
    if isFloat text then
        Just (toFloatUnsafe text)

    else
        Nothing


fromFloat : Float -> String
fromFloat =
    Elm.Kernel.String.fromNumber


fromChar : Char -> String
fromChar char =
    fromList [ char ]


cons : Char -> String -> String
cons char text =
    append (fromChar char) text


uncons : String -> Maybe ( Char, String )
uncons text =
    case toList text of
        [] ->
            Nothing

        char :: rest ->
            Just ( char, fromList rest )


map : (Char -> Char) -> String -> String
map func text =
    fromList (List.map func (toList text))


filter : (Char -> Bool) -> String -> String
filter isGood text =
    fromList (List.filter isGood (toList text))


foldl : (Char -> b -> b) -> b -> String -> b
foldl func acc text =
    List.foldl func acc (toList text)


foldr : (Char -> b -> b) -> b -> String -> b
foldr func acc text =
    List.foldr func acc (toList text)


any : (Char -> Bool) -> String -> Bool
any isGood text =
    List.any isGood (toList text)


all : (Char -> Bool) -> String -> Bool
all isGood text =
    List.all isGood (toList text)


toUpper : String -> String
toUpper text =
    map Char.toUpper text


toLower : String -> String
toLower text =
    map Char.toLower text


padLeft : Int -> Char -> String -> String
padLeft n char text =
    append (repeat (n - length text) (fromChar char)) text


padRight : Int -> Char -> String -> String
padRight n char text =
    append text (repeat (n - length text) (fromChar char))


pad : Int -> Char -> String -> String
pad n char text =
    let
        half =
            Basics.toFloat (n - length text) / 2
    in
    append (repeat (ceiling half) (fromChar char))
        (append text (repeat (floor half) (fromChar char)))


dropSpaces : List Char -> List Char
dropSpaces chars =
    case chars of
        [] ->
            []

        char :: rest ->
            if isSpace char then
                dropSpaces rest

            else
                chars


trimLeft : String -> String
trimLeft text =
    fromList (dropSpaces (toList text))


trimRight : String -> String
trimRight text =
    fromList (List.reverse (dropSpaces (List.reverse (toList text))))


trim : String -> String
trim text =
    trimLeft (trimRight text)
