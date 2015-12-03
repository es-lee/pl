(*
 * SNU 4190.310 Programming Languages
 * Homework "Exceptions are sugar" Skeleton
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

open Xexp

let count = ref 0

let new_name () =
  let _ = count := !count + 1 in
  "x_" ^ (string_of_int !count)

(* TODO : Implement this function *)
let removeExn : xexp -> xexp = fun e ->
  let k = new_name () in
  let h = new_name () in
  match e with
  | Num n -> Fn (k, Fn (h, App (Var k, Num n)))
  | Var x -> Fn (k, Fn (h, App (Var k, Var x)))
  | Fn (arg, body) ->
    let k' = new_name () in
    let h' = new_name () in
      Fn (k,
        Fn (h,
          App (Var k,
            Fn (arg,
              Fn (Var k',
                Fn (Var h',
                  App (App (removeExn (body), k'), h')))))))
  | App (fn, arg) ->
    let f = new_name () in
    let v = new_name () in
      Fn (k,
        Fn (h,
          App (App (removeExn (fn),
               Fn (f, App (App (removeExn (arg),
                           Fn (v,
                              App (App (App (f, v), k), h)
                                )), h)
                  )),
              h)
          )
        )
  | If (cond, t, f)
  | Equal (e1, e2) ->
    let v1 = new_name () in
    let v2 = new_name () in
    Fn (k,
      Fn (h,
        App (removeExn (e1),
            Fn (v1, App (removeExn (e2), Fn (v2, App (Var k, Equal (v1, v2))))))
  | Raise exp
  | Handle (exp, i, exp2)
  e
