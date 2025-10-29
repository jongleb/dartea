sum: Int
sum = 1 + 2 - 1 + xxx 80

id: a -> a
id a = a

type alias X = String

test =
    let x = 2 in 
    x + id 3

kekkk = sum + test

test2: Int -> Int -> a -> { a : { b : { c : { d : { e : { f : { g : { h : Int } } } } } } } } -> String
test2 a b c kek = 
    let x = 3 in
    let y = 4 in 
    let z = 2 in
    let result = x + a in
    let result2 = case b of 
                    2 -> 2
                    _ -> 2
    in
    let x1 = x + kek.a.b.c.d.e.f.g.h + 1 in
    concat "" ""


test2Result = test2 1 2 3 { a = { b = { c = { d = { e = { f = { g = { h = 10 } } } } } } } }

testAgain x = 
    case x of
        E C -> 1
        E D -> 3
        F (B "x") -> 4
        _ -> 5

testCaseMultipleBranches someValue =
    case someValue of
        3 -> 1
        4 -> 2
        _ -> 3


xxx x = x |> plus 1 |> plus 2 |> plus 3 |> int_to_string |> id |> concat "Value is: "

square: Int -> Int
square =
  \n -> n * n

testagain = sum |> square |> \n -> n + 1 |> int_to_string

origin = { x = 0, y = 0 }

abcdefg = { l = origin.x + origin.y }

compose f g = \x -> f (g x)

squareTwoTimes = compose square square

fn x y r = 
    let abcd = x + 1 in
    let g = case abcd of
                2 -> 3
                _ -> 4
    in
    let g1 = case y of 
                Just 1 -> 2
                Just _ -> 3
                None -> 4
    in
    let result = g + x in
    let b = r.w.e.r.t.y + 2 in
    let idb = id b in
    let result1 = compose square square 2 in
    result1
