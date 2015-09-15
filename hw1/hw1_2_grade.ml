open CommonGrade
open Hw1_2

let foo = fun n -> n * 2
let bar = fun n -> n * n

let _ = check (fun () -> sigma (3, 1, foo) = 0)
let _ = check (fun () -> sigma (4, 2, bar) = 0)
let _ = check (fun () -> sigma (8, 8, foo) = 16)
let _ = check (fun () -> sigma (3, 4, bar) = 25)
let _ = check (fun () -> sigma (1, 10, foo) = 110)
let _ = check (fun () -> sigma (1, 10, bar) = 5 * 7 * 11)
let _ = check (fun () -> sigma (5, 10, foo) = 90)
let _ = check (fun () -> sigma (1, 100, foo) = 10100)
