(*
 * SNU 4190.310 Programming Languages
 * K-- to SM5 translator skeleton code
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

open K
open Sm5
module Translator = struct

  (* TODO : complete this function  *)
  let rec trans : K.program -> Sm5.command = function
    | K.NUM i -> [Sm5.PUSH (Sm5.Val (Sm5.Z i))]
    | K.TRUE -> [Sm5.PUSH (Sm5.Val (Sm5.B true))]
    | K.FALSE -> [Sm5.PUSH (Sm5.Val (Sm5.B false))]
    | K.UNIT -> [Sm5.PUSH (Sm5.Val (Sm5.Unit))]
    | K.VAR i -> [Sm5.PUSH (Sm5.Id i); Sm5.LOAD]
    | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [Sm5.ADD]
    | K.SUB (e1, e2) -> trans e1 @ trans e2 @ [Sm5.SUB]
    | K.MUL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.MUL]
    | K.DIV (e1, e2) -> trans e1 @ trans e2 @ [Sm5.DIV]
    | K.EQUAL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.EQ]
    | K.LESS (e1, e2) -> trans e1 @ trans e2 @ [Sm5.LESS]
    | K.NOT e -> trans e @ [Sm5.NOT]
    | K.ASSIGN (id, exp) -> trans exp @ [Sm5.PUSH (Sm5.Id id); Sm5.STORE; Sm5.PUSH (Sm5.Id id); Sm5.LOAD]
    | K.SEQ (e1, e2)-> trans e1 @ trans e2
    | K.IF (e1, e2, e3) -> trans e1 @ [Sm5.JTR (trans e2, trans e3)]
    | K.WHILE (e1, e2) -> trans e1 @ failwith "Unimplemented"
    | K.FOR (id, e1, e2, e3) -> failwith "Unimplemented"
    | K.LETV (x, e1, e2) ->
      trans e1 @ [Sm5.MALLOC; Sm5.BIND x; Sm5.PUSH (Sm5.Id x); Sm5.STORE] @
      trans e2 @ [Sm5.UNBIND; Sm5.POP]
    | K.LETF (id1, id2, e1, e2) -> failwith "Unimplemented"
    | K.CALLV (id, exp) -> failwith "Unimplemented"
    | K.CALLR (id1, id2)-> failwith "Unimplemented"
    | K.READ x -> [Sm5.GET; Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
    | K.WRITE e -> trans e @ [Sm5.MALLOC; Sm5.BIND "α"; Sm5.PUSH (Sm5.Id "α"); Sm5.Store;
                              Sm5.PUSH (Sm5.Id "α"); Sm5.PUSH (Sm5.Id "α"); Sm5.Load;
                              Sm5.PUT; Sm5.Load; Sm5.UNBIND; Sm5.POP]
    | _ -> failwith "Unimplemented"
end
