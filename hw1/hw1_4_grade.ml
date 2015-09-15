open CommonGrade
open Hw1_4

let f1 = TRUE
let f2 = FALSE
let f3 = NOT f1
let f4 = ANDALSO (NOT f2, ANDALSO (f3, f1))
let f5 = ORELSE (ORELSE (f3, f1), f4)
let f6 = IMPLY (f4, f5)
let f7 = IMPLY (f5, ORELSE (f4, FALSE))
let f8 = ORELSE (IMPLY (NOT f6, f2), ANDALSO (ORELSE (f3, NOT f4), NOT f7))
let f9 = LESS (NUM 1, NUM 2)
let fa = LESS (PLUS (NUM 1, NUM 2), MINUS (NUM 0, NUM 121))
let fb =
  LESS
    (MINUS
      (PLUS (NUM 5, MINUS (NUM 1, NUM 21)),
       MINUS (NUM 0, NUM 100)), NUM 2)

let _ = check (fun () -> eval f1 = true)
let _ = check (fun () -> eval f2 = false)
let _ = check (fun () -> eval f3 = false)
let _ = check (fun () -> eval f4 = false)
let _ = check (fun () -> eval f5 = true)
let _ = check (fun () -> eval f6 = true)
let _ = check (fun () -> eval f7 = false)
let _ = check (fun () -> eval f8 = true)
let _ = check (fun () -> eval f9 = true)
let _ = check (fun () -> eval fa = false)
let _ = check (fun () -> eval fb = false)

