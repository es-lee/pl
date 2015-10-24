
let check_cnt = ref 0

let check (f: unit -> bool): unit =
  check_cnt := !check_cnt + 1;
  print_int !check_cnt;
  print_string " :\t";
  print_endline (if f () then "O" else "X")
