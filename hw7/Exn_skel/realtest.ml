let exn_magic_no = 201511

let test_run pgm_str =
  (* Creating program *)
  let lexbuf = Lexing.from_string pgm_str in
  let pgm = Parser.program Lexer.start lexbuf in
  let desugared_pgm = Desugar.removeExn pgm in
  let _ = assert (Xexp.is_sugarless desugared_pgm) in
  match (Xexp.run desugared_pgm) with
  | Xexp.Val (Xexp.N n) -> n
  | _ -> -1
