{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/core -- https://github.com/elm/core
   Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module List exposing
    ( singleton, repeat, range, cons
    , map, indexedMap, foldl, foldr, filter, filterMap
    , length, reverse, member, all, any, maximum, minimum, sum, product
    , append, concat, concatMap, intersperse, map2, map3, map4, map5
    , sort, sortBy, sortWith
    , isEmpty, head, tail, take, drop, partition, unzip
    )

import Basics exposing (..)
import Maybe exposing (Maybe(..))


singleton : a -> List a
singleton value =
    [ value ]


repeat : Int -> a -> List a
repeat n value =
    repeatHelp [] n value


repeatHelp : List a -> Int -> a -> List a
repeatHelp result n value =
    if n <= 0 then
        result

    else
        repeatHelp (cons value result) (n - 1) value


range : Int -> Int -> List Int
range lo hi =
    rangeHelp lo hi []


rangeHelp : Int -> Int -> List Int -> List Int
rangeHelp lo hi list =
    if lo <= hi then
        rangeHelp lo (hi - 1) (cons hi list)

    else
        list


cons : a -> List a -> List a
cons first rest =
    first :: rest


foldl : (a -> b -> b) -> b -> List a -> b
foldl func acc list =
    case list of
        [] ->
            acc

        x :: xs ->
            foldl func (func x acc) xs


foldr : (a -> b -> b) -> b -> List a -> b
foldr func acc list =
    foldl func acc (reverse list)


reverse : List a -> List a
reverse list =
    Elm.Kernel.List.reverse list


map : (a -> b) -> List a -> List b
map f xs =
    Elm.Kernel.List.map f xs


indexedMap : (Int -> a -> b) -> List a -> List b
indexedMap f xs =
    map2 f (range 0 (length xs - 1)) xs


filter : (a -> Bool) -> List a -> List a
filter isGood list =
    Elm.Kernel.List.filter isGood list


filterMap : (a -> Maybe b) -> List a -> List b
filterMap f xs =
    foldr (maybeCons f) [] xs


maybeCons : (a -> Maybe b) -> a -> List b -> List b
maybeCons f mx acc =
    case f mx of
        Just value ->
            cons value acc

        Nothing ->
            acc


length : List a -> Int
length xs =
    Elm.Kernel.List.length xs


member : a -> List a -> Bool
member x xs =
    any (\a -> a == x) xs


all : (a -> Bool) -> List a -> Bool
all isOkay list =
    not (any (\a -> not (isOkay a)) list)


any : (a -> Bool) -> List a -> Bool
any isOkay list =
    case list of
        [] ->
            False

        x :: xs ->
            if isOkay x then
                True

            else
                any isOkay xs


maximum : List comparable -> Maybe comparable
maximum list =
    case list of
        x :: xs ->
            Just (foldl max x xs)

        [] ->
            Nothing


minimum : List comparable -> Maybe comparable
minimum list =
    case list of
        x :: xs ->
            Just (foldl min x xs)

        [] ->
            Nothing


sum : List number -> number
sum numbers =
    foldl (\x acc -> x + acc) 0 numbers


product : List number -> number
product numbers =
    foldl (\x acc -> x * acc) 1 numbers


append : List a -> List a -> List a
append xs ys =
    case ys of
        [] ->
            xs

        _ ->
            foldr cons ys xs


concat : List (List a) -> List a
concat lists =
    foldr append [] lists


concatMap : (a -> List b) -> List a -> List b
concatMap f list =
    concat (map f list)


intersperse : a -> List a -> List a
intersperse sep xs =
    case xs of
        [] ->
            []

        hd :: tl ->
            let
                step x rest =
                    cons sep (cons x rest)

                spersed =
                    foldr step [] tl
            in
            cons hd spersed


map2 : (a -> b -> result) -> List a -> List b -> List result
map2 f xs ys =
    reverse (map2Help f xs ys [])


map2Help : (a -> b -> result) -> List a -> List b -> List result -> List result
map2Help f xs ys acc =
    case xs of
        [] ->
            acc

        x :: xrest ->
            case ys of
                [] ->
                    acc

                y :: yrest ->
                    map2Help f xrest yrest (cons (f x y) acc)


map3 : (a -> b -> c -> result) -> List a -> List b -> List c -> List result
map3 f xs ys zs =
    map2 (\g z -> g z) (map2 f xs ys) zs


map4 :
    (a -> b -> c -> d -> result)
    -> List a
    -> List b
    -> List c
    -> List d
    -> List result
map4 f xs ys zs ws =
    map2 (\g w -> g w) (map3 f xs ys zs) ws


map5 :
    (a -> b -> c -> d -> e -> result)
    -> List a
    -> List b
    -> List c
    -> List d
    -> List e
    -> List result
map5 f xs ys zs ws vs =
    map2 (\g v -> g v) (map4 f xs ys zs ws) vs


sort : List comparable -> List comparable
sort xs =
    sortWith compare xs


sortBy : (a -> comparable) -> List a -> List a
sortBy toKey xs =
    sortWith (\a b -> compare (toKey a) (toKey b)) xs


sortWith : (a -> a -> Order) -> List a -> List a
sortWith ordering xs =
    case xs of
        [] ->
            []

        _ :: [] ->
            xs

        _ ->
            let
                wanted =
                    length xs // 2
            in
            mergeWith ordering
                (sortWith ordering (take wanted xs))
                (sortWith ordering (drop wanted xs))


mergeWith : (a -> a -> Order) -> List a -> List a -> List a
mergeWith ordering left right =
    reverse (mergeHelp ordering left right [])


mergeHelp : (a -> a -> Order) -> List a -> List a -> List a -> List a
mergeHelp ordering left right acc =
    case left of
        [] ->
            foldl cons acc right

        l :: lrest ->
            case right of
                [] ->
                    foldl cons acc left

                r :: rrest ->
                    if ordering r l == LT then
                        mergeHelp ordering left rrest (cons r acc)

                    else
                        mergeHelp ordering lrest right (cons l acc)


isEmpty : List a -> Bool
isEmpty xs =
    case xs of
        [] ->
            True

        _ ->
            False


head : List a -> Maybe a
head list =
    case list of
        x :: _ ->
            Just x

        [] ->
            Nothing


tail : List a -> Maybe (List a)
tail list =
    case list of
        _ :: xs ->
            Just xs

        [] ->
            Nothing


take : Int -> List a -> List a
take n list =
    reverse (takeHelp n list [])


takeHelp : Int -> List a -> List a -> List a
takeHelp n list acc =
    if n <= 0 then
        acc

    else
        case list of
            [] ->
                acc

            x :: xs ->
                takeHelp (n - 1) xs (cons x acc)


drop : Int -> List a -> List a
drop n list =
    if n <= 0 then
        list

    else
        case list of
            [] ->
                list

            _ :: xs ->
                drop (n - 1) xs


partition : (a -> Bool) -> List a -> ( List a, List a )
partition pred list =
    let
        step x ( trues, falses ) =
            if pred x then
                ( cons x trues, falses )

            else
                ( trues, cons x falses )
    in
    foldr step ( [], [] ) list


unzip : List ( a, b ) -> ( List a, List b )
unzip pairs =
    let
        step ( x, y ) ( xs, ys ) =
            ( cons x xs, cons y ys )
    in
    foldr step ( [], [] ) pairs
