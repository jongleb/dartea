sum = 1 + 2 - 1

test =
    let x = 2 in 
    x + 3

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

testCaseMultipleBranches someValue =
    case someValue of
        3 -> 1
        4 -> 2
        _ -> 3
