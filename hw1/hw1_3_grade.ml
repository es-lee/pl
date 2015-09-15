open CommonGrade
open Hw1_3

let f1 = fun x -> 2 + x
let f2 = fun x -> fun y -> x * y

let _ = check (fun () -> (iter (10, f1)) 3 = 23)
let _ = check (fun () -> (iter (2, f1)) 121 = f1 (f1 121))
let _ = check (fun () -> (iter (3, f1)) 177 = f1 (f1 (f1 177)))
let _ = check (fun () -> ((iter (2, f2 2)) 4 = f2 2 (f2 2 4)))
