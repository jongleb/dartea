type Maybe = 
    Just a 
    | Nothing

type Test3 
    = A1 
    | B2

type alias Kek = Int

type alias Test a = Maybe a

type alias Test2 = 
    Int -> Int -> Int -> { a : { b : { c : { d : { e : { f : { g : { h : Kek } } } } } } } } -> String

type TestEnum
    = C
    | D

type TestEnum2
    = E TestEnum
    | F String



plus : Int -> Int -> Int
plus a b = 
    a + b



id : a -> a
id a = 
    a

sum : Int
sum = 
    1 + 2 - 1

kekk : Test3 -> Int
kekk x = 
    case x of
        A1 -> 
            1
        
        B2 -> 
            2

test : Int
test = 
    let 
        x = 2 
    in 
    x + id 3

kekkk : Int
kekkk = 
    sum + test

test2 : Int -> Int -> Int -> { a : { b : { c : { d : { e : { f : { g : { h : Kek } } } } } } } } -> String
test2 a b c kek = 
    let 
        x = 3
        y = 4
        z = 2
        result = x + a
        result2 = 
            case b of
                2 -> 
                    2
                
                _ -> 
                    2
        
        x1 = x + kek.a.b.c.d.e.f.g.h + 1
    in 
    "" ++ ""

test2Result : String
test2Result = 
    test2 1 2 3 
        { a = 
            { b = 
                { c = 
                    { d = 
                        { e = 
                            { f = 
                                { g = 
                                    { h = 10 
                                    } 
                                } 
                            } 
                        } 
                    } 
                } 
            } 
        }

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

testCaseMultipleBranches : Int -> Int
testCaseMultipleBranches someValue = 
    case someValue of
        3 -> 
            1
        
        4 -> 
            2
        
        _ -> 
            3

xxx : Int -> Int
xxx x = 
    x
        |> plus 1
        |> plus 2
        |> plus 3

square : Int -> Int
square = 
    \n -> n * n

testagain : Int
testagain = 
    sum
        |> square
        |> (\n -> n + 1)

origin : { x : Int, y : Int }
origin = 
    { x = 0, y = 0 }

abcdefg : { l : Int }
abcdefg = 
    { l = origin.x + origin.y }

compose : (b -> c) -> (a -> b) -> (a -> c)
compose f g = 
    \x -> f (g x)

squareTwoTimes : Int -> Int
squareTwoTimes = 
    compose square square

fn : Int -> Maybe Int -> { w : { e : { r : { t : { y : Int } } } } } -> Int
fn x y r = 
    let 
        abcd = x + 1
        
        g = 
            case abcd of
                2 -> 
                    3
                
                _ -> 
                    4
        
        g1 = 
            case y of
                Just 1 -> 
                    2
                
                Just _ -> 
                    3
                
                Nothing -> 
                    4
        
        result = g + x
        
        b = r.w.e.r.t.y + 2
        
        idb = id b
        
        result1 = compose square square 2
    in 
    result1
