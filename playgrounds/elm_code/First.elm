
type TestEnum
    = C
    | D


type TestEnum2
    = E TestEnum
    | F String


testAgain : TestEnum2 -> Int
testAgain x =
    case x of
        E C ->
            1

        E D ->
            3

        F "x" ->
            4

        _ ->
            5


result : Int
result =
    testAgain (F "lol")
