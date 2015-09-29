open CommonGrade
open Hw2_2

let rec crazy2val : crazy2 -> int =
  fun c ->
    match c with
    | NIL -> 0
    | ZERO n -> 2 * crazy2val n
    | ONE n -> 1 + 2 * crazy2val n
    | MONE n -> -1 + 2 * crazy2val n

let c0 = ZERO NIL
let c1 = ONE NIL
let c2 = ONE (ZERO (ONE NIL))
let c3 = ONE (MONE NIL)
let c4 = ONE (MONE (ZERO (MONE NIL)))
let rec c5 n =
  if n = 0 then NIL
  else ONE (MONE (c5 (n-1)))
let rec c6 n =
  if n = 0 then NIL
  else MONE (ZERO (ONE (c6 (n-1))))
let c7 = MONE (ZERO (ZERO (ZERO NIL)))
let c8 =
  ZERO (ONE (ONE (ZERO (MONE (ONE (ONE (ZERO (ONE (ZERO (MONE NIL))))))))))


let _ = check (fun () -> crazy2val (crazy2add (c0, c0)) = 0)
let _ = check (fun () -> crazy2val (crazy2add (c0, c1)) = 1)
let _ = check (fun () -> crazy2val (crazy2add (c1, c0)) = 1)
let _ = check (fun () -> crazy2val (crazy2add (c0, c2)) = 5)
let _ = check (fun () -> crazy2val (crazy2add (c2, c0)) = 5)
let _ = check (fun () -> crazy2val (crazy2add (c0, c3)) = -1)
let _ = check (fun () -> crazy2val (crazy2add (c3, c0)) = -1)
let _ = check (fun () -> crazy2val (crazy2add (c0, c4)) = -9)
let _ = check (fun () -> crazy2val (crazy2add (c4, c0)) = -9)

let _ = check (fun () -> crazy2val (crazy2add (c1, c1)) = 2)
let _ = check (fun () -> crazy2val (crazy2add (c1, c2)) = 6)
let _ = check (fun () -> crazy2val (crazy2add (c2, c1)) = 6)
let _ = check (fun () -> crazy2val (crazy2add (c1, c3)) = 0)
let _ = check (fun () -> crazy2val (crazy2add (c3, c1)) = 0)
let _ = check (fun () -> crazy2val (crazy2add (c1, c4)) = -8)
let _ = check (fun () -> crazy2val (crazy2add (c4, c1)) = -8)

let _ = check (fun () -> crazy2val (crazy2add (c2, c2)) = 10)
let _ = check (fun () -> crazy2val (crazy2add (c2, c3)) = 4)
let _ = check (fun () -> crazy2val (crazy2add (c3, c2)) = 4)
let _ = check (fun () -> crazy2val (crazy2add (c2, c4)) = -4)
let _ = check (fun () -> crazy2val (crazy2add (c4, c2)) = -4)

let _ = check (fun () -> crazy2val (crazy2add (c3, c3)) = -2)
let _ = check (fun () -> crazy2val (crazy2add (c3, c4)) = -10)
let _ = check (fun () -> crazy2val (crazy2add (c4, c3)) = -10)

let _ = check (fun () -> crazy2val (crazy2add (c4, c4)) = -18)

let _ = check (fun () -> crazy2val (crazy2add (c5 1, c5 1)) = -2)
let _ = check (fun () -> crazy2val (crazy2add (c5 2, c5 1)) = -6)
let _ = check (fun () -> crazy2val (crazy2add (c5 3, c5 2)) = -26)
let _ = check (fun () -> crazy2val (crazy2add (c5 2, c5 5)) = -346)

let _ = check (fun () -> crazy2val (crazy2add (c6 1, c5 1)) = 2)
let _ = check (fun () -> crazy2val (crazy2add (c5 4, c6 2)) = -58)
let _ = check (fun () -> crazy2val (crazy2add (c6 3, c5 4)) = 134)
let _ = check (fun () ->
  crazy2val (crazy2add (crazy2add (c5 4, c6 3), (crazy2add (c6 3, c5 5)))) = 12)

let _ = check (fun () -> crazy2val (crazy2add (c7, c8)) = (-683))
