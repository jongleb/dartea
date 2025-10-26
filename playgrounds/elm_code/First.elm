sum = 1 + 2 - 1 + xxx 80

id a = a

test =
    let x = 2 in 
    x + id 3

kekkk = sum + test

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
