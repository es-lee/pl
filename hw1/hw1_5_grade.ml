open CommonGrade
open Hw1_5

let rec int_of_nat =
  fun n ->
    match n with
    | ZERO -> 0
    | SUCC m -> (int_of_nat m) + 1

let rec nat_of_int =
  fun i ->
    match i with
    | 0 -> ZERO
    | _ -> SUCC (nat_of_int (i - 1))

let _ = check (fun () -> int_of_nat (natadd (nat_of_int 0, nat_of_int 0)) = 0)
let _ = check (fun () -> int_of_nat (natadd (nat_of_int 2, nat_of_int 0)) = 2)
let _ = check (fun () -> int_of_nat (natadd (nat_of_int 0, nat_of_int 3)) = 3)
let _ = check (fun () -> int_of_nat (natadd (nat_of_int 1, nat_of_int 5)) = 6)
let _ = check (fun () -> int_of_nat (natadd (nat_of_int 3, nat_of_int 3)) = 6)
let _ = check (fun () -> int_of_nat (natadd (nat_of_int 12, nat_of_int 7)) = 19)
let _ = check (fun () -> int_of_nat (natadd (nat_of_int 34, nat_of_int 19)) =53)

let _ = check (fun () -> int_of_nat (natmul (nat_of_int 0, nat_of_int 0)) = 0)
let _ = check (fun () -> int_of_nat (natmul (nat_of_int 2, nat_of_int 0)) = 0)
let _ = check (fun () -> int_of_nat (natmul (nat_of_int 0, nat_of_int 3)) = 0)
let _ = check (fun () -> int_of_nat (natmul (nat_of_int 1, nat_of_int 5)) = 5)
let _ = check (fun () -> int_of_nat (natmul (nat_of_int 3, nat_of_int 3)) = 9)
let _ = check (fun () -> int_of_nat (natmul (nat_of_int 11, nat_of_int 7)) = 77)
let _ = check (fun () -> int_of_nat (natmul (nat_of_int 8, nat_of_int 12)) =96)
