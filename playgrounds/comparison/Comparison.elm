module Comparison exposing (report, checks)

import Char


type Color
    = Red
    | Green
    | Blue


type Shape
    = Dot
    | Line Int Int
    | Named String


type Tree
    = Leaf
    | Node Tree Int Tree


type Boxed
    = Boxed Float


type alias Point =
    { x : Int
    , y : String
    }


numbers : Bool
numbers =
    1 == 1


floats : Bool
floats =
    1.5 < 2.5


chars : Bool
chars =
    'a' < 'b'


strings : Bool
strings =
    "apple" < "pear"


booleans : Bool
booleans =
    True == True


units : Bool
units =
    () == ()


enums : Bool
enums =
    Red == Red


enumsDiffer : Bool
enumsDiffer =
    Red == Green


records : Bool
records =
    { x = 1, y = "a" } == { x = 1, y = "a" }


recordsDiffer : Bool
recordsDiffer =
    { x = 1, y = "a" } == { x = 2, y = "a" }


tuples : Bool
tuples =
    ( 1, "a" ) == ( 1, "a" )


tuplesOrdered : Bool
tuplesOrdered =
    ( 1, "a" ) < ( 1, "b" )


tuplesLexicographic : Bool
tuplesLexicographic =
    ( 1, "z" ) < ( 2, "a" )


nestedTuples : Bool
nestedTuples =
    ( 1, ( "a", 'c' ) ) == ( 1, ( "a", 'c' ) )


listsOfNumbers : Bool
listsOfNumbers =
    [ 1, 2, 3 ] == [ 1, 2, 3 ]


listsOfNumbersOrdered : Bool
listsOfNumbersOrdered =
    [ 1, 2 ] < [ 1, 3 ]


listsOfNumbersPrefix : Bool
listsOfNumbersPrefix =
    [ 1 ] < [ 1, 2 ]


emptyIsSmallest : Bool
emptyIsSmallest =
    [] < [ 1 ]


listsOfStrings : Bool
listsOfStrings =
    [ "x", "y" ] == [ "x", "y" ]


listsOfRecords : Bool
listsOfRecords =
    [ { x = 1, y = "a" } ] == [ { x = 1, y = "a" } ]


variants : Bool
variants =
    Line 1 2 == Line 1 2


variantsDiffer : Bool
variantsDiffer =
    Line 1 2 == Line 1 3


variantsAcrossTags : Bool
variantsAcrossTags =
    Named "a" == Line 1 2


variantsNullary : Bool
variantsNullary =
    Dot == Dot


taggedOmitted : Bool
taggedOmitted =
    Boxed 1.5 == Boxed 1.5


recursive : Bool
recursive =
    Node (Node Leaf 1 Leaf) 2 Leaf == Node (Node Leaf 1 Leaf) 2 Leaf


recursiveDiffer : Bool
recursiveDiffer =
    Node (Node Leaf 1 Leaf) 2 Leaf == Node (Node Leaf 9 Leaf) 2 Leaf


maybes : Bool
maybes =
    Just 1 == Just 1


maybesAgainstNothing : Bool
maybesAgainstNothing =
    Just 1 == Nothing


listsOfVariants : Bool
listsOfVariants =
    [ Dot, Line 1 2 ] == [ Dot, Line 1 2 ]


smallestNumber : Int
smallestNumber =
    min 4 2


largestWord : String
largestWord =
    max "apple" "pear"


comparedNumbers : Order
comparedNumbers =
    compare 1 2


comparedTuples : Order
comparedTuples =
    compare ( 1, "b" ) ( 1, "a" )


comparedLists : Order
comparedLists =
    compare [ 1, 2 ] [ 1, 3 ]


smallestTuple : ( Int, String )
smallestTuple =
    min ( 2, "a" ) ( 1, "z" )


appendedStrings : String
appendedStrings =
    "hello " ++ "world"


appendedLists : List Int
appendedLists =
    [ 1, 2 ] ++ [ 3 ]


appendedGenerically : appendable -> appendable -> appendable
appendedGenerically one other =
    one ++ other


sameAnything : a -> a -> Bool
sameAnything one other =
    one == other


smallestComparable : comparable -> comparable -> comparable
smallestComparable one other =
    if one < other then
        one

    else
        other


grinning : Int
grinning =
    Char.toCode '\u{1F600}'


tree : Int
tree =
    Char.toCode '木'


checks : List Bool
checks =
    [ numbers
    , floats
    , chars
    , strings
    , booleans
    , units
    , enums
    , not enumsDiffer
    , records
    , not recordsDiffer
    , tuples
    , tuplesOrdered
    , tuplesLexicographic
    , nestedTuples
    , listsOfNumbers
    , listsOfNumbersOrdered
    , listsOfNumbersPrefix
    , emptyIsSmallest
    , listsOfStrings
    , listsOfRecords
    , variants
    , not variantsDiffer
    , not variantsAcrossTags
    , variantsNullary
    , taggedOmitted
    , recursive
    , not recursiveDiffer
    , maybes
    , not maybesAgainstNothing
    , listsOfVariants
    , smallestNumber == 2
    , largestWord == "pear"
    , comparedNumbers == LT
    , comparedTuples == GT
    , comparedLists == LT
    , smallestTuple == ( 1, "z" )
    , appendedStrings == "hello world"
    , appendedLists == [ 1, 2, 3 ]
    , appendedGenerically "a" "b" == "ab"
    , appendedGenerically [ 1 ] [ 2 ] == [ 1, 2 ]
    , sameAnything [ 1, 2 ] [ 1, 2 ]
    , not (sameAnything { x = 1, y = "a" } { x = 2, y = "a" })
    , smallestComparable 4 2 == 2
    , smallestComparable "b" "a" == "a"
    , grinning == 128512
    , tree == 0x6728
    ]


allTrue : List Bool -> Bool
allTrue flags =
    case flags of
        [] ->
            True

        flag :: rest ->
            flag && allTrue rest


report : String
report =
    if allTrue checks then
        "all instances agree"

    else
        "SOMETHING DISAGREES"
