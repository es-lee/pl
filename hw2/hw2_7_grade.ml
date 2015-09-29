open CommonGrade
open Hw2_7
open Zexpr

module ValidateZexpr = (Zexpr : ZEXPR)

let test =
  fun expr str ->
    print_string (str ^ " = ") ; print_value (eval (emptyEnv, expr))
let ex_test =
  fun expr ->
    try test expr "Error" with
    | _ -> print_string "Error\n"

let xpx = PLUS (VAR "x", VAR "x")
let xmx = MULT (VAR "x", VAR "x")
let lyxx = LET ("y", xpx, MULT (NUM (-1), VAR "y"))
let zyyx = LET ("z", xpx,
  LET ("y", MAX [NUM (-3); NUM (-2); NUM (-1)],
    LET ("y", PLUS (VAR "y", VAR "z"),
      LET ("x", xmx, DIVIDE (MULT (VAR "x", VAR "y"), NUM 2)))))

let _ = test (NUM 7) "7"
let _ = test (LET ("x", NUM 12, VAR "x")) "12"
let _ = test (LET ("x", NUM 10, MINUS (NUM 8, VAR "x"))) "-2"
let _ = test (LET ("x", NUM 10, PLUS (NUM (-8), VAR "x"))) "2"
let _ = test (LET ("x", NUM 2, DIVIDE (NUM 8, VAR "x"))) "4"

let _ = test (MAX []) "0"
let _ = test (MAX [NUM 1; NUM 3; NUM 5; NUM 7; NUM 9; NUM 2; NUM 4; NUM 6; NUM 8; NUM 10; NUM 135]) "135"
let _ = test (MAX [NUM min_int]) (string_of_int min_int)
let _ = test (LET ("x", NUM (-1), MAX [VAR "x"; NUM (-2)])) "-1"
let _ = test (LET ("x", NUM 2,
  MAX [NUM 1; NUM (-3); MAX []; lyxx; xpx])) "4"

let _ = test (LET ("y", NUM 1,
  PLUS (LET ("y", NUM 2, PLUS (VAR "y", VAR "y")), VAR "y"))) "5"
let _ = test (LET ("x", NUM 1,
  PLUS (LET ("y", NUM 2, PLUS (VAR "x", VAR "y")), VAR "x"))) "4"
let _ = test (LET ("x", NUM 4,
  MULT (LET ("x", xpx, xpx), LET ("x", NUM 2, LET ("x", xpx, xpx))))) "128"
let _ = test (LET ("x", NUM 2,
  MULT (LET ("x", NUM 1, MAX [NUM 0; VAR "x"; NUM 3]), VAR "x"))) "6"
let _ = test (LET ("x", MAX [NUM 3; NUM 8; NUM 5; NUM 2; NUM 8; NUM 7; NUM 6],
  LET ("y", PLUS (xmx, xpx), PLUS (LET ("x", NUM 1, lyxx), zyyx)))) "478"

let _ = ex_test (LET ("y", NUM 2, VAR "x"))
let _ = ex_test (LET ("x", NUM 1,
  PLUS (LET ("y", NUM 2, PLUS (VAR "x", VAR "y")), VAR "y")))
let _ = ex_test (LET ("x", MAX [],
  (LET ("y", NUM 128, DIVIDE (VAR "y", VAR "x")))))
let _ = ex_test (LET ("x", MAX [],
  (LET ("y", NUM 0, DIVIDE (VAR "y", VAR "x")))))
