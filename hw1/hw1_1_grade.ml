open CommonGrade
open Hw1_1

let l1 = [5; 3; 1]
let l2 = [4; 2; 0]
let l3 = [10; 7; 6]
let l4 = [11; 9; 8]

let _ = check (fun () -> (merge (l1, l2)) = [5; 4; 3; 2; 1; 0])
let _ = check (fun () -> (merge (l2, l1)) = [5; 4; 3; 2; 1; 0])
let _ = check (fun () -> (merge (l3, l4)) = [11; 10; 9; 8; 7; 6])
let _ = check (fun () -> (merge (l4, l3)) = [11; 10; 9; 8; 7; 6])
let _ = check (fun () -> (merge (l1, l4)) = [11; 9; 8; 5; 3; 1])
let _ = check (fun () -> (merge ((merge (l1, l2)), l4)) = [11; 9; 8; 5; 4; 3; 2; 1; 0])
let _ = check (fun () -> (merge (merge (l1, l2), merge (l4, l3))) = [11; 10; 9; 8; 7; 6; 5; 4; 3; 2; 1; 0])
