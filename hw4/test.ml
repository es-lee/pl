open Hw4_2

let check_exception m =
  try (let _ = getReady m in false) with IMPOSSIBLE -> true

let check_answer m ans =
  let res = List.sort Pervasives.compare (getReady m) in
  let ans = List.sort Pervasives.compare ans in
  res = ans

(* 조교 test case *)
let _ = print_endline ("TA testcase")

let m1 = Branch(End(NameBox"x"),Branch(End(NameBox"y"),Branch(End(NameBox"z"),End(StarBox))))
let ans1 = [Bar; Node (Bar, Bar)]
let _ = print_endline (string_of_bool (check_answer m1 ans1))

let m2 = Guide("x",Guide("y",Branch(End(NameBox"x"),Branch(End(NameBox"y"),End(NameBox"x")))))
let ans2 = [Node (Bar, Bar); Node (Node (Bar, Bar), Bar)]
let _ = print_endline (string_of_bool (check_answer m2 ans2))

let m3 = Branch (Branch (Branch (Guide ("2", Guide ("1", Branch (End (NameBox "1"), End (NameBox "2")))), Guide ("3", End (NameBox "3")) ), Guide ("4", End (NameBox "4"))), End (NameBox "5"))
let ans3 = [Bar; Node (Bar, Bar); Node (Node (Bar, Bar), Node (Bar, Bar))]
let _ = print_endline (string_of_bool (check_answer m3 ans3))

let m4 = Branch(Branch(Branch(Branch(End (NameBox "a"), End(NameBox "b")), End(NameBox "c")),Branch(Branch(End (NameBox "d"), End(NameBox "e")), End(NameBox "f"))),Branch(End (NameBox "f"), End StarBox))
let ans4 = [Bar; Node (Bar, Bar); Node (Bar, Node (Node (Bar, Bar), Bar)); Node (Bar, Node (Bar, Node (Bar, Node (Bar, Bar)))) ]
let _ = print_endline (string_of_bool (check_answer m4 ans4))

let m5 = Branch (Guide ("x", Branch (End (NameBox "x"), End (NameBox "x"))), End StarBox)
let _ = print_endline (string_of_bool (check_exception m5))
