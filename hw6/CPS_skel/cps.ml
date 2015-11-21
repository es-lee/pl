(*
 * SNU 4190.310 Programming Languages
 * Continuation Passing Style Conversion Skeleton
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

open M0

let count = ref 0

let new_name () =
  let _ = count := !count + 1 in
  "x_" ^ (string_of_int !count)

let rec alpha_conv exp subs =
  match exp with
  | Num n -> Num n
  | Var x -> (try Var (List.assoc x subs) with Not_found -> Var x)
  | Fn (x, e) ->
    let x' = new_name () in
    let subs' = (x, x') :: subs in
    Fn (x', alpha_conv e subs')
  | App (e1, e2) -> App (alpha_conv e1 subs, alpha_conv e2 subs)
  | Rec (f, x, e) ->
    let x' = new_name () in
    let f' = new_name () in
    let subs' = (f, f') :: (x, x') :: subs in
    Rec (f', x', alpha_conv e subs')
  | Ifz (e1, e2, e3) ->
    Ifz (alpha_conv e1 subs, alpha_conv e2 subs, alpha_conv e3 subs)
  | Add (e1, e2) -> Add (alpha_conv e1 subs, alpha_conv e2 subs)
  | Pair (e1, e2) -> Pair (alpha_conv e1 subs, alpha_conv e2 subs)
  | Fst e -> Fst (alpha_conv e subs)
  | Snd e -> Snd (alpha_conv e subs)

(* TODO : Complete this function *)
let rec cps' exp =
  let k = new_name () in
  match exp with
  (* Constant expressions *)
  | Num n -> Fn (k, App (Var k, Num n))
  | Var x -> Fn (k, App (Var k, Var x))
  | Fn (x, e) -> Fn (k, App (Var k, Fn (x, e)))
  | Rec (f, x, e) -> Fn (k, App (Var k, Rec (f, x, e)))
  (* Non constant expressions *)
  | App (e1, e2) ->
    let f = new_name () in
    let v = new_name () in
    Fn (k,
        App (cps' e1,
            Fn (f,
                App (cps' e2,
                    Fn (v,
                        App (Var k, App (Var f, Var v))
                        )
                    )
                )
            )
        )
  | Ifz (e1, e2, e3) ->
    let v = new_name () in
    let v1 = new_name () in
    let v2 = new_name () in
    Fn (k,
        App (cps' e1,
            Fn (v,
                App (cps' e2,
                     Fn (v1,
                        App (cps' e3,
                            Fn (v2,
                                App (Var k, Ifz (Var v, Var v1, Var v2))
                                )
                            )
                        )
                     )
                )
            )
        )
  | Add (e1, e2) ->
    let v1 = new_name () in
    let v2 = new_name () in
    Fn (k,
        App (cps' e1,
            Fn (v1,
                App (cps' e2,
                    Fn (v2,
                        App (Var k, Add (Var v1, Var v2))
                        )
                    )
                )
            )
        )
  | Pair (e1, e2) ->
    let v = new_name () in
    let w = new_name () in
    Fn (k,
        App (cps' e1,
            Fn (v,
                App (cps' e2,
                    Fn (w,
                        App (Var k, Pair (Var v, Var w))
                        )
                    )
                )
            )
        )
  | Fst e ->
    let v = new_name () in
    Fn (k,
        App (cps' e,
            Fn (v,
                App (Var k, Fst (Var v))
                )
            )
        )
  | Snd e ->
    let v = new_name () in
    Fn (k,
        App (cps' e,
            Fn (v,
                App (Var k, Snd (Var v))
                )
            )
        )

let cps exp = cps' (alpha_conv exp [])

