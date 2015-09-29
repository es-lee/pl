open CommonGrade
open Hw2_1

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

let _ = check (fun () -> crazy2val c1 = 1)
let _ = check (fun () -> crazy2val c2 = 5)
let _ = check (fun () -> crazy2val c3 = -1)
let _ = check (fun () -> crazy2val c4 = -9)
let _ = check (fun () -> crazy2val (c5 1) = -1)
let _ = check (fun () -> crazy2val (c5 2) = -5)
let _ = check (fun () -> crazy2val (c5 3) = -21)
let _ = check (fun () -> crazy2val (c5 4) = -85)
let _ = check (fun () -> crazy2val (c5 5) = -341)
let _ = check (fun () -> crazy2val (c6 1) = 3)
let _ = check (fun () -> crazy2val (c6 2) = 27)
let _ = check (fun () -> crazy2val (c6 3) = 219)

