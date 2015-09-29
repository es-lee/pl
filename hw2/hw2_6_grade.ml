open CommonGrade
open Hw2_6

module ValidIntListQ = (IntListQ : Queue)

let q1 = IntListQ.emptyQ
let q2 = IntListQ.enQ (q1, [1])
let q3 = IntListQ.enQ (q2, [2])
let q4 = IntListQ.enQ (q3, [3])
let (x1, q5) = IntListQ.deQ q4
let q6 = IntListQ.enQ (q5, [4])
let q7 = IntListQ.enQ (q6, [5])
let q8 = IntListQ.enQ (q7, [6])
let (x2, q9) = IntListQ.deQ q8
let (x3, qa) = IntListQ.deQ q9
let (x4, qb) = IntListQ.deQ qa
let (x5, qc) = IntListQ.deQ qb
let (x6, qd) = IntListQ.deQ qc

let _ = check (fun () -> fst (IntListQ.deQ q2) = [1])
let _ = check (fun () -> fst (IntListQ.deQ q5) = [2])
let _ = check (fun () -> fst (IntListQ.deQ (snd (IntListQ.deQ q5))) = [3])
let _ = check (fun () -> x1 = [1])
let _ = check (fun () -> x2 = [2])
let _ = check (fun () -> x3 = [3])
let _ = check (fun () -> x4 = [4])
let _ = check (fun () -> x5 = [5])
let _ = check (fun () -> x6 = [6])
let _ = check (fun () -> qd = q1)
let _ = check (fun () ->
  try IntListQ.deQ qd = ([], q1) with IntListQ.EMPTY_Q -> true | _ -> false)

