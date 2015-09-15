open CommonGrade
open Hw1_2

let foo n = n * 2

let _ = check (fun () -> (sigma (1, 10, foo) = 110))
let _ = check (fun () -> (sigma (5, 10, foo) = 90))
let _ = check (fun () -> (sigma (1, 100, foo) = 10100))
let _ = check (fun () -> (sigma (8, 8, foo) = 16))
