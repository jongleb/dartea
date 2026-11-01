{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/core -- https://github.com/elm/core
   Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Set exposing
  ( Set
  , empty, singleton, insert, remove
  , isEmpty, member, size
  , union, intersect, diff
  , toList, fromList
  , map, foldl, foldr, filter, partition
  )


import Basics exposing (..)
import Dict
import List
import Maybe exposing (Maybe(..))


type Set t =
  Set_elm_builtin (Dict.Dict t ())


empty : Set a
empty =
  Set_elm_builtin Dict.empty


singleton : comparable -> Set comparable
singleton key =
  Set_elm_builtin (Dict.singleton key ())


insert : comparable -> Set comparable -> Set comparable
insert key (Set_elm_builtin dict) =
  Set_elm_builtin (Dict.insert key () dict)


remove : comparable -> Set comparable -> Set comparable
remove key (Set_elm_builtin dict) =
  Set_elm_builtin (Dict.remove key dict)


isEmpty : Set a -> Bool
isEmpty (Set_elm_builtin dict) =
  Dict.isEmpty dict


member : comparable -> Set comparable -> Bool
member key (Set_elm_builtin dict) =
  Dict.member key dict


size : Set a -> Int
size (Set_elm_builtin dict) =
  Dict.size dict


union : Set comparable -> Set comparable -> Set comparable
union (Set_elm_builtin dict1) (Set_elm_builtin dict2) =
  Set_elm_builtin (Dict.union dict1 dict2)


intersect : Set comparable -> Set comparable -> Set comparable
intersect (Set_elm_builtin dict1) (Set_elm_builtin dict2) =
  Set_elm_builtin (Dict.intersect dict1 dict2)


diff : Set comparable -> Set comparable -> Set comparable
diff (Set_elm_builtin dict1) (Set_elm_builtin dict2) =
  Set_elm_builtin (Dict.diff dict1 dict2)


toList : Set a -> List a
toList (Set_elm_builtin dict) =
  Dict.keys dict


fromList : List comparable -> Set comparable
fromList list =
  List.foldl insert empty list


foldl : (a -> b -> b) -> b -> Set a -> b
foldl func initialState (Set_elm_builtin dict) =
  Dict.foldl (\key _ state -> func key state) initialState dict


foldr : (a -> b -> b) -> b -> Set a -> b
foldr func initialState (Set_elm_builtin dict) =
  Dict.foldr (\key _ state -> func key state) initialState dict


map : (comparable -> comparable2) -> Set comparable -> Set comparable2
map func set =
  fromList (foldl (\x xs -> func x :: xs) [] set)


filter : (comparable -> Bool) -> Set comparable -> Set comparable
filter isGood (Set_elm_builtin dict) =
  Set_elm_builtin (Dict.filter (\key _ -> isGood key) dict)


partition : (comparable -> Bool) -> Set comparable -> (Set comparable, Set comparable)
partition isGood (Set_elm_builtin dict) =
  let
    (dict1, dict2) =
      Dict.partition (\key _ -> isGood key) dict
  in
    (Set_elm_builtin dict1, Set_elm_builtin dict2)
