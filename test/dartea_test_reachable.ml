open OUnit2

let measured folder =
  Reachable.shown folder
    (Reachable.measure
       (Sample.delivered ~delivery:Dartea.Delivery.default
          (Sample.compiled_in
             (Filename.concat Sample.playground_root folder))))

let expected =
  [
    "browser 2/20 declarations, 92/3509 bytes";
    "comparison 97/114 declarations, 9120/12739 bytes";
    "counter 72/128 declarations, 7384/13446 bytes";
    "crossmod 10/28 declarations, 951/4368 bytes";
    "currying 25/42 declarations, 1582/4967 bytes";
    "elm_code 135/149 declarations, 22248/24775 bytes";
    "fib 4/22 declarations, 530/3947 bytes";
  ]

let test_playgrounds _ =
  assert_equal ~printer:(String.concat "\n")
    (List.sort String.compare expected)
    (List.sort String.compare (List.map measured Sample.playgrounds))

let suite = [ "playgrounds" >:: test_playgrounds ]
