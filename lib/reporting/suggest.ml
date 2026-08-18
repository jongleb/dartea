let lowered written = String.lowercase_ascii written

let distance one other =
  let one = lowered one and other = lowered other in
  let these = String.length one and those = String.length other in
  let table = Array.make_matrix (these + 1) (those + 1) 0 in
  for index = 0 to these do
    table.(index).(0) <- index
  done;
  for index = 0 to those do
    table.(0).(index) <- index
  done;
  for row = 1 to these do
    for column = 1 to those do
      let cost = if Char.equal one.[row - 1] other.[column - 1] then 0 else 1 in
      let step =
        min
          (min (table.(row - 1).(column) + 1) (table.(row).(column - 1) + 1))
          (table.(row - 1).(column - 1) + cost)
      in
      let swapped =
        if
          row > 1 && column > 1
          && Char.equal one.[row - 1] other.[column - 2]
          && Char.equal one.[row - 2] other.[column - 1]
        then min step (table.(row - 2).(column - 2) + 1)
        else step
      in
      table.(row).(column) <- swapped
    done
  done;
  table.(these).(those)

let sorted ~target written =
  List.stable_sort
    (fun one other -> Int.compare (distance target one) (distance target other))
    written

let nearest ~target written = sorted ~target written
