open CommonGrade
open Hw4_2

let check_exception m =
  check (fun () -> try (let _ = getReady m in false) with IMPOSSIBLE -> true)

let check_answer m ans =
  let res = List.sort Pervasives.compare (getReady m) in
  let ans = List.sort Pervasives.compare ans in
  check (fun () -> res = ans)

let m1 = Guide("a",End(NameBox"a"))
let ans1 = [Bar]

let m2 = Branch(Guide("x",End(NameBox"x")),End(StarBox))
let ans2 = [Bar]

let m3 = Guide("a",Guide("b",Branch(End(NameBox"b"),End(NameBox"a"))))
let ans3 = [Bar; Node (Bar, Bar)]

let m4 = Branch(Guide("y",End(NameBox"y")),Guide("x",End(NameBox"x")))
let ans4 = [Bar; Node (Bar, Bar)]

let m5 = Branch(Branch(End(NameBox"a"),End(StarBox)),End(NameBox"b"))
let ans5 = [Bar; Node (Bar, Node (Bar, Bar))]

let m6 = Guide("x",Branch(End(NameBox"y"),Branch(End(NameBox"x"),End(StarBox))))
let ans6 = [Bar; Node (Bar, Bar)]

let m7 = Branch(Guide("y", End(NameBox"y")), Guide("x", Branch(End(NameBox"x"), End StarBox)))
let ans7 = [Bar;Node(Bar,Bar);Node(Node(Bar,Bar),Bar)]

let m8 = Branch (End(NameBox"x"), Branch(End(NameBox"y"),Branch(End(NameBox"z"),End(StarBox))))
let ans8 = [Bar; Node (Bar, Bar)]

let m9 = Guide("x", Guide("y", Branch(End(NameBox"x"),Branch(End(NameBox"y"),End(NameBox"x")))))
let ans9 = [Node (Bar, Bar); Node (Node (Bar, Bar), Bar)]

let m10 = Guide ("x", Guide ("y", Branch(End(NameBox"y"), Branch (End(NameBox "x"), End StarBox))))
let ans10 = [Bar; Node (Bar, Bar)]

let m11 = Guide ("a", Branch (Branch (End (NameBox "a"), End (StarBox)), Guide ("b", End (NameBox "b"))))
let ans11 = [Bar; Node (Bar, Node (Node (Bar, Bar), Bar))]

let m12 = Branch(Branch(End(NameBox"p"),Branch(End(NameBox"q"),End(StarBox))),Guide("r",End(NameBox"r")))
let ans12 = [Bar; Node (Bar, Bar); Node (Bar, Node (Node (Bar, Bar), Bar))]

let m13 = Branch(Branch(Guide("x",End(NameBox"x")),Guide("y",End(NameBox"y"))),Branch(End(NameBox"z"),End(StarBox)))
let ans13 = [Bar; Node (Bar, Bar)]

let m14 = Guide ("x", Guide ("y", Guide ("z", Branch (Branch (End (NameBox "x"), End (NameBox "y")), End (NameBox "z")))))
let ans14 = [Bar; Node (Bar, Node (Bar, Bar))]

let m15 = Branch(Guide("z",End(NameBox"z")),Guide("x",Guide("y",Branch(End(NameBox"x"),Branch(End(NameBox"y"),End(StarBox))))))
let ans15 = [Bar; Node (Bar, Bar); Node (Node (Bar, Bar), Node (Node (Bar, Bar), Bar))]

let m16 = Branch (Branch (Branch (Guide("t",Guide("o",Branch(End(NameBox"o"),End(NameBox"t")))), Guide("h",End(NameBox"h"))), Guide("f",End(NameBox"f"))), End(NameBox"v"))
let ans16 = [Bar; Node (Bar, Bar); Node (Node (Bar, Bar), Node (Bar, Bar))]

let m17 = Branch (Branch (End (NameBox "x"), End (NameBox "y")), Guide ("y", Branch (End (NameBox "y"), End StarBox)))
let ans17 = [Bar; Node (Bar, Bar); Node (Node (Bar, Bar), Node (Node (Node (Bar, Bar), Bar), Bar))]

let m18 = Branch(Guide("a",Guide("b",Branch(End(NameBox"a"),End(NameBox"b")))),Guide("c",Guide("d",Branch(End(NameBox"d"),End(NameBox"c")))))
let ans18 = [Bar; Node (Bar, Bar); Node (Bar, Node (Node (Bar, Bar), Bar))]

let m19 = Branch(Branch(Branch(Guide("x",Guide("y",Guide("z",Branch(Branch (End (NameBox "x"),End (NameBox "y")),End (NameBox "z"))))),End (NameBox "a")),End (NameBox "b")),End (NameBox "c"))
let ans19 = [Bar; Node(Bar,Node(Bar,Bar))]

let m20 = Branch (Branch (Branch (Branch (End(NameBox"a"), End(NameBox"b")), End(NameBox"c")), Branch (Branch(End(NameBox"d"),End(NameBox"e")), End(NameBox"f"))), Branch(End(NameBox"b"),End(StarBox)))
let ans20 = [Bar; Node (Bar, Bar); Node (Bar, Node (Bar, Bar)); Node (Node (Bar, Bar), Node (Bar, Node (Bar, Node (Bar, Bar))))]

let m21 = Branch (Branch (Branch (Branch (End(NameBox"a"), End(NameBox"b")), End(NameBox"c")), Branch (Branch(End(NameBox"d"),End(NameBox"e")), End(NameBox"f"))), Branch(End(NameBox"c"),End(StarBox)))
let ans21 = [Bar; Node (Bar, Bar); Node (Bar, Node (Bar, Bar)); Node (Bar, Node (Node (Bar, Bar), Node (Bar, Node (Bar, Bar))))]

let m22 = Branch (Branch (Branch (Branch (End(NameBox"a"), End(NameBox"b")), End(NameBox"c")), Branch (Branch(End(NameBox"d"),End(NameBox"e")), End(NameBox"f"))), Branch(End(NameBox"d"),End(StarBox)))
let ans22 = [Bar; Node (Bar, Node (Bar,Bar)); Node (Bar, Node (Bar, Node (Bar, Node (Node (Bar,Bar), Bar))))]

let m23 = Branch (Branch (Branch (Branch (End(NameBox"a"), End(NameBox"b")), End(NameBox"c")), Branch (Branch(End(NameBox"d"),End(NameBox"e")), End(NameBox"f"))), Branch(End(NameBox"e"),End(StarBox)))
let ans23 = [Bar; Node(Bar,Bar); Node(Node(Bar,Bar),Node(Bar,Bar)); Node(Bar,Node(Bar,Node(Bar,Node(Bar,Bar))))]

let m24 = Branch (Branch (Branch (Branch (End(NameBox"a"), End(NameBox"b")), End(NameBox"c")), Branch (Branch(End(NameBox"d"),End(NameBox"e")), End(NameBox"f"))), Branch(End(NameBox"f"),End(StarBox)))
let ans24 = [Bar; Node (Bar, Bar); Node (Bar, Node (Node (Bar, Bar), Bar)); Node (Bar, Node (Bar, Node (Bar, Node (Bar, Bar))))]

let m25 = Branch(Guide ("x", (Guide ("y", Guide ("z", Guide ("w", Branch (Branch (End (NameBox "x"), End (NameBox "y")), Branch (End (NameBox "z"), End (NameBox "w")))))))), Guide ("a", Branch (End (NameBox "a"),End (NameBox "b"))))
let ans25 = [Bar; Node(Bar,Bar); Node(Bar,Node(Bar,Bar)); Node(Node(Bar,Node(Bar,Bar)),Node(Bar,Bar))]

let ex1 = Guide("x",Branch(End(StarBox),End(NameBox"x")))
let ex2 = Branch(Guide("x",Branch(End(NameBox"x"),End(NameBox"x"))),End(StarBox))
let ex3 = Branch(Guide ("x", Branch (End (NameBox "x"), End (NameBox "x"))),Guide ("y", Branch (End (NameBox "y"), End (NameBox "y"))))
let ex4 = Branch(Guide ("z", End (NameBox "z")), Branch(Guide ("x", Branch (End (NameBox "x"), End (NameBox "x"))),Guide ("y", Branch (End (NameBox "y"), End (NameBox "y")))))
let ex5 = Branch(Branch(End(NameBox"q"),End(NameBox"p")),Guide("q",Branch(End(NameBox"p"),End(NameBox"q"))))
let ex6 = Branch(Guide("b",Branch(End (NameBox "a"), End(NameBox "b"))), Guide("a", Branch(End (NameBox "b"), End(NameBox "a"))))
let ex7 = Branch(Guide("x",Branch(End(NameBox"y"), End(NameBox"x"))),Guide("y",Branch(Branch(End(NameBox"z"),Branch(End(NameBox"y"),End(NameBox"w"))),End(NameBox"u"))))
let ex8 = Branch (Branch (Branch (Branch (End(NameBox"a"), End(NameBox"b")), End(NameBox"c")), Branch (Branch(End(NameBox"d"),End(NameBox"e")), End(NameBox"f"))), Branch(End(NameBox"a"),End(StarBox)))
let ex9 = Branch(Guide("x",(Guide("y",Guide("z",Guide ("w",Branch(Branch(End(NameBox"x"),End(NameBox"y")),Branch(End(NameBox"z"),End(NameBox"w")))))))),End StarBox)
let ex10 = Branch(Guide("x",(Guide("y",Guide("z",Guide ("w",Branch(Branch(End(NameBox"x"),End(NameBox"y")),Branch(End(NameBox"z"),End(NameBox"w")))))))),Branch(End(NameBox "a"),End(StarBox)))

let b1 = Branch ((Guide ("Bill", (End (NameBox "Bill")))) , (Branch ((Branch ((Guide ("Claudette", (Branch ((Branch ((End (NameBox "Claudette")) , (End (NameBox "Danny")))) , (Branch ((End (NameBox "Erika")) , (Guide ("Fred", (End (NameBox "Fred")))))))))) , (Branch ((Branch ((Branch ((Branch ((End (NameBox "Grace")) , (End (NameBox "Henri")))) , (Branch ((End (NameBox "Ida")) , (End (NameBox "Joaquin")))))) , (End (NameBox "Kate")))) , (Branch ((End (NameBox "Larry")) , (Branch ((End (NameBox "Mindy")) , (Guide ("Nicholas", (End (NameBox "Nicholas")))))))))))) , (Branch ((Branch ((End (NameBox "Odette")) , (Guide ("Peter", (Guide ("Rose", (Branch ((End (NameBox "Rose")) , (End (NameBox "Sam")))))))))) , (Branch ((Branch ((Guide ("Teresa", (End (NameBox "Teresa")))) , (End (NameBox "Victor")))) , (End (NameBox "Wanda")))))))))
let bns1 = [Bar; Node(Bar,Bar); Node(Bar,Node(Bar,Node(Bar,Bar))); Node(Bar,Node(Bar,Node(Bar,Node(Bar,Node(Bar,Node(Bar,Node(Bar,Bar))))))); Node(Node(Bar,Bar),Bar); Node(Node(Bar,Node(Node(Bar,Bar),Bar)),Node(Bar,Bar))]

let _ = print_endline "Main tests"

let _ = check_answer m1 ans1
let _ = check_answer m2 ans2
let _ = check_answer m3 ans3
let _ = check_answer m4 ans4
let _ = check_answer m5 ans5
let _ = check_answer m6 ans6
let _ = check_answer m7 ans7
let _ = check_answer m8 ans8
let _ = check_answer m9 ans9
let _ = check_answer m10 ans10
let _ = check_answer m11 ans11
let _ = check_answer m12 ans12
let _ = check_answer m13 ans13
let _ = check_answer m14 ans14
let _ = check_answer m15 ans15
let _ = check_answer m16 ans16
let _ = check_answer m17 ans17
let _ = check_answer m18 ans18
let _ = check_answer m19 ans19
let _ = check_answer m20 ans20
let _ = check_answer m21 ans21
let _ = check_answer m22 ans22
let _ = check_answer m23 ans23
let _ = check_answer m24 ans24
let _ = check_answer m25 ans25

let _ = print_endline "Exception tests"

let _ = check_exception ex1
let _ = check_exception ex2
let _ = check_exception ex3
let _ = check_exception ex4
let _ = check_exception ex5
let _ = check_exception ex6
let _ = check_exception ex7
let _ = check_exception ex8
let _ = check_exception ex9
let _ = check_exception ex10

let _ = print_endline "Big test"

let _ = check_answer b1 bns1

