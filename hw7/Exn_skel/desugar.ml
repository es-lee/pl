(*
 * SNU 4190.310 Programming Languages
 * Homework "Exceptions are sugar" Skeleton
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

open Xexp

(* TODO : Implement this function *)
let rec removeExn exp : xexp -> xexp =
  match exp with
  | Num n -> exp
  | Var str -> exp
  | Fn (fname, fbody) -> Fn (fname, removeExn fbody)
  | App (f, arg) -> App (removeExn f, removeExn arg)
  | If (cond, xthen, xelse) -> IF (removeExn cond, removeExn xthen, removeExn xelse)
  | Equal (e1, e2) -> Equal (removeExn e1, removeExn e2)
  | Raise err -> (* TODO *) Num 1
  | Handle (e, num, handle) -> (* TODO *) Num 1
