(*
 * SNU 4190.310 Programming Languages
 * Homework "Exceptions are sugar" Skeleton
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

open Xexp

let count = ref 0

let new_name () =
  let _ = count := !count + 1 in
  "galaxy_" ^ (string_of_int !count)

let rec alpha_conv e subs =
  match e with
  | Num n -> Num n
  | Var x -> (try Var (List.assoc x subs) with Not_found -> Var x)
  | Fn (arg, body) ->
      let x = new_name () in
      Fn (x, alpha_conv body (arg, x)::subs)
  | App (fn, arg) -> App (alpha_conv fn subs, alpha_conv arg subs)
  | If (cond, t, f) -> If (alpha_conv cond subs, alpha_conv t subs, alpha_conv f subs)
  | Equal (e1, e2) -> Equal (alpha_conv e1 subs, alpha_conv e2 subs)
  | Raise exp -> Raise (alpha_conv exp subs)
  | Handle (exp, i, exp2) -> Handle (alpha_conv exp subs, i, alpha_conv exp2 subs)

(* TODO : Implement this function *)
let rec cps (e: xexp): xexp =
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
              Fn (k',
                Fn (h',
                  App (App (cps body, Var k'), Var h')))))))
  | App (fn, arg) ->
    let f = new_name () in
    let v = new_name () in
      Fn (k,
        Fn (h,
          App (App (cps (fn),
               Fn (f, App (App (cps arg,
                           Fn (v,
                              App (App (App (Var f, Var v), Var k), Var h)
                                )), Var h)
                  )),
              Var h)
          )
        )
  | If (cond, t, f) ->
    let v = new_name () in
    let v2 = new_name () in
    let v3 = new_name () in
    Fn (k,
    Fn (h,
        App (App (cps cond,
            Fn (v, If (Var v,
                        App (App (cps t, Fn (v2, App (Var k, Var v2))), Var h),
                        App (App (cps f, Fn (v3, App (Var k, Var v3))), Var h)
                        )
                )
            ),
            Var h)
        )
    )
  | Equal (e1, e2) ->
    let v1 = new_name () in
    let v2 = new_name () in
    Fn (k,
      Fn (h,
        App (App (cps e1,
                  Fn (v1, App (App (cps e2, Fn (v2, App (Var k, Equal (Var v1, Var v2)))), Var h)))
                ,
              Var h)))
  | Raise exp ->
    Fn (k,
      Fn (h,
        App (App (cps exp, Var h), Var h)))
  | Handle (exp, i, exp2) ->
    let x = new_name () in
    let handle = If (Equal (Var x, Num i),
                     App (App (cps exp2, Var k), Var h),
                     App (Var h, Var x)) in
    Fn (k,
      Fn (h,
        App (App (cps exp, Var k),
             Fn (x, handle))))

let removeExn (e: xexp): xexp =
  let x = new_name () in
  let magic = Fn (x, Num 201511) in
  let k = Fn ("#x", Var "#x") in
  App (App (cps (alpha_conv e []), k), magic)
