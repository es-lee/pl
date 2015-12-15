(*
 * SNU 4190.310 Programming Languages 2015 Fall
 * Type Checker Skeleton
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

open M

type var = string

type cons =
  | Equal
  | Write

type typ =
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
  | TCvar of var * cons
  (* Modify, or add more if needed *)

type typ_scheme =
  | SimpleTyp of typ
  | GenTyp of (var list * typ)

type typ_env = (M.id * typ_scheme) list


(* for DEBUG *)
let debug = true

exception DO_YOURSELF

(* stack indentation level *)
let cnt = ref 0
let inc () = cnt := !cnt + 1
let dec () = cnt := !cnt - 1

let print_indent () =
  let rec pp i =
    if i <= 1 then ()
    else let _ = print_string " | " in pp (i - 1)
  in let _ = pp !cnt in print_string " "

(* push/pop call stack *)
let call str =
  if debug then
    let _ = inc () in
    let _ = print_indent () in
    print_string (str ^ "\n")
  else ()
let ret str =
  if debug then
    let _ = print_indent () in
    let _ = print_string (str ^ "\n") in
    dec ()
  else ()

(* print debug message *)
let deb str =
  if debug then
    let _ = print_indent () in
    print_string ("* " ^ str)
  else ()
let prt : typ -> unit =
  let rec iter : typ -> unit =
    fun t ->
      match t with
      | TInt -> print_string "Int"
      | TBool -> print_string "Bool"
      | TString -> print_string "String"
      | TPair (t1, t2) ->
          let _ = print_string "(" in
          let _ = iter t1 in
          let _ = print_string ", " in
          let _ = iter t2 in
          print_string ")"
      | TFun (t1, t2) ->
          let _ = print_string "(" in
          let _ = iter t1 in
          let _ = print_string " → " in
          let _ = iter t2 in
          print_string ")"
      | TLoc t ->
          let _ = iter t in
          print_string "_Loc"
      | TVar v -> print_string ("Var " ^ v)
      | TCvar (v, Equal) -> print_string ("Equal " ^ v)
      | TCvar (v, Write) -> print_string ("Write " ^ v)
    in
  fun t ->
    if debug then
      let _ = iter t in
      print_string "\n"
    else ()

let count = ref 0

let new_var () =
  let _ = count := !count +1 in
  "x_" ^ (string_of_int !count)

(* Definitions related to free type variable *)

let union_ftv ftv_1 ftv_2 =
  let ftv_1' = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1 in
  ftv_1' @ ftv_2

let sub_ftv ftv_1 ftv_2 =
  List.filter (fun v -> not (List.mem v ftv_2)) ftv_1

let rec ftv_of_typ : typ -> var list = function
  | TInt | TBool | TString -> []
  | TPair (t1, t2) -> union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TLoc t -> ftv_of_typ t
  | TFun (t1, t2) ->  union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TVar v -> [v]
  | TCvar (v, _) -> [v]

let ftv_of_scheme : typ_scheme -> var list = function
  | SimpleTyp t -> ftv_of_typ t
  | GenTyp (alphas, t) -> sub_ftv (ftv_of_typ t) alphas

let ftv_of_env : typ_env -> var list = fun tyenv ->
  List.fold_left
    (fun acc_ftv (id, tyscm) -> union_ftv acc_ftv (ftv_of_scheme tyscm))
    [] tyenv

(* Generalize given type into a type scheme *)
let generalize : typ_env -> typ -> typ_scheme = fun tyenv t ->
  let env_ftv = ftv_of_env tyenv in
  let typ_ftv = ftv_of_typ t in
  let ftv = sub_ftv typ_ftv env_ftv in
  if List.length ftv = 0 then
    SimpleTyp t
  else
    GenTyp(ftv, t)

(* Definitions related to substitution *)

type subst = typ -> typ

let empty_subst : subst = fun t -> t

let make_subst : var -> typ -> subst = fun x t ->
  let rec subs t' =
    match t' with
    | TVar x' -> if (x = x') then t else t'
    | TCvar (x', Equal) ->
      if (x = x') then
      (match t with
      | TVar a -> TCvar (a, Equal)
      | _ -> t)
      else t'
    | TCvar (x', Write) ->
      if (x = x') then
      (match t with
      | TVar a-> TCvar (a, Write)
      | _ -> t)
      else t'
    | TPair (t1, t2) -> TPair (subs t1, subs t2)
    | TLoc t'' -> TLoc (subs t'')
    | TFun (t1, t2) -> TFun (subs t1, subs t2)
    | TInt | TBool | TString -> t'
  in subs

let (@@) s1 s2 = (fun t -> s1 (s2 t)) (* substitution composition *)

let subst_scheme : subst -> typ_scheme -> typ_scheme = fun subs tyscm ->
  match tyscm with
  | SimpleTyp t -> SimpleTyp (subs t)
  | GenTyp (alphas, t) ->
    (* S (\all a.t) = \all b.S{a->b}t  (where b is new variable) *)
    let betas = List.map (fun _ -> new_var()) alphas in
    let s' =
      List.fold_left2
        (fun acc_subst alpha beta -> make_subst alpha (TVar beta) @@ acc_subst)
        empty_subst alphas betas
    in
    GenTyp (betas, subs (s' t))

let subst_env : subst -> typ_env -> typ_env = fun subs tyenv ->
  List.map (fun (x, tyscm) -> (x, subst_scheme subs tyscm)) tyenv

let rec inside id typ =
  match typ with
  | TVar v -> if id = v then true else false
  | TPair (t1, t2) ->
      let x = inside id t1 in
      let y = inside id t2 in
      x||y
  | TLoc typ -> inside id typ
  | TFun (t1, t2) ->
      let x = inside id t1 in
      let y = inside id t2 in
      x||y
  | _ -> false

let rec unify (t1:typ) (t2:typ) : (typ -> typ) =
  let rec unify_p (a, b) (c, d) =
    let s1 = unify a c in
    let s2 = unify (s1 b) (s1 d) in
    s2 @@ s1 in
  match (t1, t2) with
  | (TVar v, _) ->
    if TVar v = t2 then empty_subst
    else if not (inside v t2) then (make_subst v t2)
    else raise (M.TypeError "unify failed 1")
  | (typ, TVar v) -> unify t2 t1
  | (TCvar (v, Equal), typ) ->
    (match typ with
    | TInt | TBool | TString | TLoc _ -> make_subst v typ
    | TCvar (_, Equal) -> make_subst v typ
    | TCvar (_, Write) -> make_subst v typ
    |_ -> raise (M.TypeError "unify failed 3")
    )
  | (typ, TCvar (v, Equal)) -> unify t2 t1
  | (TCvar (v, Write), typ) ->
    (match typ with
    | TInt | TBool | TString -> make_subst v typ
    | TCvar (_, Write) -> make_subst v typ
    |_ -> raise (M.TypeError "unify failed 3")
    )
  | (typ, TCvar (v, Write)) -> unify t2 t1
  | (TPair (a, b), TPair (c, d)) -> unify_p (a, b) (c, d)
  | (TLoc typ, TLoc typ') -> unify typ typ'
  | (TFun (a, b), TFun (c, d)) -> unify_p (a, b) (c, d)
  | _ ->
    if t1 = t2 then empty_subst
    else raise (M.TypeError "unify failed 2")

let rec expansive (exp:M.exp) =
  match exp with
  | M.CONST const -> false
  | M.VAR id -> false
  | M.FN (id, exp) -> false
  | M.APP (fn, arg) -> true
  | M.LET (M.REC (f, x, e1), e2) -> expansive e1 || expansive e2
  | M.LET (M.VAL (x, e1), e2) -> expansive e1 || expansive e2
  | M.IF (cond, etrue, efalse) -> expansive cond || expansive etrue || expansive efalse
  | M.BOP (bop, eleft, eright) -> expansive eleft || expansive eright
  | M.READ -> false
  | M.WRITE exp -> expansive exp
  | M.MALLOC exp -> true
  | M.ASSIGN (e1, e2) -> expansive e1 || expansive e2
  | M.BANG exp -> expansive exp
  | M.SEQ (e1, e2) -> expansive e1 || expansive e2
  | M.PAIR (eleft, eright) -> expansive eleft || expansive eright
  | M.FST exp -> expansive exp
  | M.SND exp -> expansive exp

let rec online (tenv:typ_env) (exp:M.exp) (typ:typ)=
  match exp with
  | M.CONST (M.S s) ->
      let _ = call "[String]" in
      let _ = deb "typ : " in
      let _ = prt typ in
      let s =
      unify typ TString
      in
      let _ = ret "[String]" in
      s
  | M.CONST (M.N n) ->
      let _ = call "[Int]" in
      let _ = deb "typ : " in
      let _ = prt typ in
      let s =
      unify typ TInt
      in
      let _ = ret "[Int]" in
      s
  | M.CONST (M.B b) ->
      let _ = call "[Bool]" in
      let _ = deb "typ : " in
      let _ = prt typ in
      let s =
      unify typ TBool
      in
      let _ = ret "[Bool]" in
      s
  | M.VAR id ->
    let _ = call ("[Var]" ^ id)  in
    let _ = deb "typ : " in
    let _ = prt typ in
    let type_scheme =
      try
        List.assoc id tenv
      with
        Not_found -> raise (M.TypeError "unbound variable")
    in
    let s =
    (match type_scheme with
    | SimpleTyp t -> unify typ t
    | GenTyp (alphas, t) ->
      let GenTyp (betas, t) = subst_scheme empty_subst type_scheme in
      unify typ t)
    in
    let _= deb "typ := " in
    let _= prt (s typ) in
    let _ = ret ("[Var]" ^ id) in
    s
  | M.FN (id, exp) ->
    let _ = call ("[Fn]" ^ id)  in
    let _ = deb "typ : " in
    let _ = prt typ in
    let b1 = TVar (new_var ()) in
    let b2 = TVar (new_var ()) in
    let s1 = unify typ (TFun (b1, b2)) in
    let tenv = (id, SimpleTyp (s1 b1))::(subst_env s1 tenv) in
    let s2 = online tenv exp (s1 b2) in
    let s =
    s2 @@ s1
    in
    let _= deb "typ := " in
    let _= prt (s typ) in
    let _ = ret ("[Fn]" ^ id) in
    s
  | M.APP (fn, arg) ->
    let _ = call "[App]" in
    let _ = deb "typ : " in
    let _ = prt typ in
    let b = TVar (new_var ()) in
    let s1 = online tenv fn (TFun (b, typ)) in
    let tenv = subst_env s1 tenv in
    let s2 = online tenv arg (s1 b) in
    let s =
    s2 @@ s1
    in
    let _= deb "typ := " in
    let _= prt (s typ) in
    let _ = ret ("[App]") in
    s
  | M.LET (M.REC (f, x, e1), e2) ->
    let _ = call "[Letrec]" in
    let _ = deb "typ : " in
    let _ = prt typ in
    let b = TVar (new_var ()) in
(*
    let tenv = (f, SimpleTyp b)::tenv in
*)
    let tenv = (f, generalize tenv b)::tenv in
    let s = online tenv (M.FN (x, e1)) b in
    let tenv = subst_env s tenv in
    let tenv = (f, generalize tenv (s b))::tenv in
    let s = (online tenv e2 (s typ)) @@ s in
    let _= deb "typ := " in
    let _= prt (s typ) in
    let _ = ret "[Letrec]" in
    s
  | M.LET (M.VAL (x, e1), e2) ->
    let _ = call ("[Let value]")  in
    let _ = deb "typ : " in
    let _ = prt typ in
    let expansive = expansive e1 in
    let b = TVar (new_var ()) in
    let s = online tenv e1 b in
    let tenv = (subst_env s tenv) in
    let tenv =
      if expansive then
        (x, SimpleTyp (s b))::tenv
      else
        (x, generalize tenv (s b))::tenv in
    let s = (online tenv e2 (s typ)) @@ s in
    let _= deb "typ := " in
    let _= prt (s typ) in
    let _ = ret "[Let value]" in
    s
  | M.IF (cond, etrue, efalse) ->
    let _ = call ("[If]")  in
    let _ = deb "typ : " in
    let _ = prt typ in
    let s = online tenv cond TBool in
    let tenv = subst_env s tenv in
    let s = (online tenv etrue (s typ)) @@ s in
    let tenv = subst_env s tenv in
    let s = (online tenv efalse (s typ)) @@ s in
    let _= deb "typ := " in
    let _= prt (s typ) in
    let _ = ret "[If]" in
    s
  | M.BOP (M.ADD, eleft, eright)
  | M.BOP (M.SUB, eleft, eright) ->
    let _ = call ("[Add/Sub]")  in
    let _ = deb "typ : " in
    let _ = prt typ in
    let s = unify typ TInt in
    let tenv = subst_env s tenv in
    let s = (online tenv eleft TInt) @@ s in
    let tenv = subst_env s tenv in
    let s = (online tenv eright TInt) @@ s in
    let _= deb "typ := " in
    let _= prt (s typ) in
    let _ = ret "[Add/Sub]" in
    s
  | M.BOP (M.AND, eleft, eright)
  | M.BOP (M.OR, eleft, eright) ->
    let _ = call ("[And/Or]")  in
    let _ = deb "typ : " in
    let _ = prt typ in
    let s = unify typ TBool in
    let tenv = subst_env s tenv in
    let s = (online tenv eleft TBool) @@ s in
    let tenv = subst_env s tenv in
    let s = (online tenv eright TBool) @@ s in
    let _= deb "typ := " in
    let _= prt (s typ) in
    let _ = ret "[And/Or]" in
    s
  | M.BOP (M.EQ, eleft, eright) ->
    let _ = call ("[Equal]")  in
    let _ = deb "typ : " in
    let _ = prt typ in
    let cvar = TCvar (new_var (), Equal) in
    let s = unify typ TBool in
    let tenv = subst_env s tenv in
    let s = (online tenv eleft (s cvar)) @@ s in
    let tenv = subst_env s tenv in
    let s = (online tenv eright (s cvar)) @@ s in
    let _= deb "typ := " in
    let _= prt (s typ) in
    let _ = ret "[Equal]" in
    s
  | M.READ ->
    let _ = call ("[Read]")  in
    let _ = deb "typ : " in
    let _ = prt typ in
    let s =
    unify typ TInt
    in
    let _= deb "typ := " in
    let _= prt (s typ) in
    let _ = ret "[Read]" in
    s
  | M.WRITE exp ->
    let _ = call ("[Write]")  in
    let _ = deb "typ : " in
    let _ = prt typ in
    let cvar = TCvar (new_var (), Write) in
    let s = unify typ cvar in
    let tenv = subst_env s tenv in
    let s = (online tenv exp (s cvar)) @@ s in
    let _= deb "typ := " in
    let _= prt (s typ) in
    let _ = ret "[Write]" in
    s
  | M.MALLOC exp ->
    let _ = call ("[Malloc]")  in
    let _ = deb "typ : " in
    let _ = prt typ in
    let b = TVar (new_var ()) in
    let s = unify typ (TLoc b) in
    let tenv = subst_env s tenv in
    let s = (online tenv exp (s b)) @@ s in
    let _= deb "typ := " in
    let _= prt (s typ) in
    let _ = ret "[Write]" in
    s
  | M.ASSIGN (e1, e2) ->
    let _ = call ("[Assign]")  in
    let _ = deb "typ : " in
    let _ = prt typ in
    let s = online tenv e1 (TLoc typ) in
    let tenv = subst_env s tenv in
    let s = (online tenv e2 (s typ)) @@ s in
    let _= deb "typ := " in
    let _= prt (s typ) in
    let _ = ret "[Assign]" in
    s
  | M.BANG exp ->
    let _ = call ("[Bang]")  in
    let _ = deb "typ : " in
    let _ = prt typ in
    let s =
    online tenv exp (TLoc typ)
    in
    let _= deb "typ := " in
    let _= prt (s typ) in
    let _ = ret "[Bang]" in
    s
  | M.SEQ (e1, e2) ->
    let _ = call ("[Seq]")  in
    let _ = deb "typ : " in
    let _ = prt typ in
    let b = TVar (new_var ()) in
    let s = online tenv e1 b in
    let tenv = subst_env s tenv in
    let s = (online tenv e2 (s typ)) @@ s in
    let _= deb "typ := " in
    let _= prt (s typ) in
    let _ = ret "[Seq]" in
    s
  | M.PAIR (eleft, eright) ->
    let _ = call ("[Pair]")  in
    let _ = deb "typ : " in
    let _ = prt typ in
    let bleft = TVar (new_var ()) in
    let bright = TVar (new_var ()) in
    let s = unify typ (TPair (bleft, bright)) in
    let tenv = subst_env s tenv in
    let s = (online tenv eleft (s bleft)) @@ s in
    let tenv = subst_env s tenv in
    let s = (online tenv eright (s bright)) @@ s in (* bright?? *)
    let _= deb "typ := " in
    let _= prt (s typ) in
    let _ = ret "[Pair]" in
    s
  | M.FST exp ->
    let _ = call ("[Fst]")  in
    let _ = deb "typ : " in
    let _ = prt typ in
    let b2 = TVar (new_var ()) in
(*    let b2 = TVar (new_var ()) in
    let s = unify typ b1 in
    let tenv = subst_env s tenv in
    let s1 = online tenv exp (TPair ((s b1), (s b2))) in
    s1 @@ s
*)
    let s = online tenv exp (TPair (typ, b2)) in
    let _= deb "typ := " in
    let _= prt (s typ) in
    let _ = ret "[Fst]" in
    s
  | M.SND exp ->
    let _ = call ("[Snd]")  in
    let _ = deb "typ : " in
    let _ = prt typ in
    let b1 = TVar (new_var ()) in
    let b2 = TVar (new_var ()) in
    let s = unify typ b2 in
    let tenv = subst_env s tenv in
    let s = (online tenv exp (TPair ((s b1), (s b2)))) @@ s in
    let _= deb "typ := " in
    let _= prt (s typ) in
    let _ = ret "[Snd]" in
    s

let rec toMtyp (typ:typ) : M.typ =
  match typ with
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TPair (t1, t2) -> M.TyPair (toMtyp t1, toMtyp t2)
  | TLoc typ -> M.TyLoc (toMtyp typ)
  | TFun (t1, t2) -> raise (M.TypeError "final type is TFun")
  | TVar v -> raise (M.TypeError "final type is TVar")
  | TCvar _-> raise (M.TypeError "final type is TCvar")

let check (exp:M.exp) : M.typ =
  let initial_type = TVar (new_var ()) in
  let s = online [] exp initial_type in
  toMtyp (s initial_type)
