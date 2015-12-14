(*
 * SNU 4190.310 Programming Languages 2015 Fall
 * Type Checker Skeleton
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

open M

type var = string

type typ =
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
  | Twrite (* const: int, bool string *)
  | Tequal (* const + loc *)
  (* Modify, or add more if needed *)

type typ_scheme =
  | SimpleTyp of typ
  | GenTyp of (var list * typ)

type typ_env = (M.id * typ_scheme) list

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
  | (typ, TVar v) -> unify (TVar v) typ
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

let rec foo (tenv:typ_env) (exp:M.exp) (typ:typ)=
  match exp with
  | M.CONST (M.S s) -> unify typ TString
  | M.CONST (M.N n) -> unify typ TInt
  | M.CONST (M.B b) -> unify typ TBool
  | M.VAR id ->
    let type_scheme = List.assoc id tenv in
    (match type_scheme with
    | SimpleTyp t -> unify typ t
    | GenTyp (alphas, t) ->
      let GenTyp (betas, t) = subst_scheme empty_subst type_scheme in
      unify typ t)
  | M.FN (id, exp) ->
    let b1 = TVar (new_var ()) in
    let b2 = TVar (new_var ()) in
    let s1 = unify typ (TFun (b1, b2)) in
    let tenv = (id, SimpleTyp (s1 b1))::(subst_env s1 tenv) in
    let s2 = foo tenv exp (s1 b2) in
    s2 @@ s1
  | M.APP (fn, arg) ->
    let b = TVar (new_var ()) in
    let s1 = foo tenv fn (TFun (b, typ)) in
    let tenv = subst_env s1 tenv in
    let s2 = foo tenv arg (s1 b) in
    s2 @@ s1
  | M.LET (M.REC (f, x, e1), e2) ->
    let b = TVar (new_var ()) in
    let tenv = (f, generalize tenv b)::tenv in
    let s = foo tenv (M.FN (x, e1)) b in
    let tenv = subst_env s tenv in
    let tenv = (f, generalize tenv (s b))::tenv in
    let s = (foo tenv e2 (s typ)) @@ s in
    s
  | M.LET (M.VAL (x, e1), e2) ->
    let expansive = expansive e1 in
    let b = TVar (new_var ()) in
    let s = foo tenv e1 b in
    let tenv = (subst_env s tenv) in
    let tenv =
      if expansive then
        (x, SimpleTyp (s b))::tenv
      else
        (x, generalize tenv (s b))::tenv in
    let s = (foo tenv e2 (s typ)) @@ s in
    s
  | M.IF (cond, etrue, efalse) ->
    let s = foo tenv cond TBool in
    let tenv = subst_env s tenv in
    let s = (foo tenv etrue (s typ)) @@ s in
    let tenv = subst_env s tenv in
    let s = (foo tenv efalse (s typ)) @@ s in
    s
  | M.BOP (M.ADD, eleft, eright)
  | M.BOP (M.SUB, eleft, eright) ->
    let s = unify typ TInt in
    let tenv = subst_env s tenv in
    let s1 = foo tenv eleft TInt in
    let s = s1 @@ s in
    let tenv = subst_env s tenv in
    let s1 = foo tenv eright TInt in
    s1 @@ s
  | M.BOP (M.AND, eleft, eright)
  | M.BOP (M.OR, eleft, eright) ->
    let s = unify typ TBool in
    let tenv = subst_env s tenv in
    let s1 = foo tenv eleft TBool in
    let s = s1 @@ s in
    let tenv = subst_env s tenv in
    let s1 = foo tenv eright TBool in
    s1 @@ s
  | M.BOP (M.EQ, eleft, eright) -> empty_subst (* TODO *)
  | M.READ -> unify typ TInt
  | M.WRITE exp ->  empty_subst (* TODO *)
  | M.MALLOC exp ->
    let b = TVar (new_var ()) in
    let s = unify typ (TLoc b) in
    let tenv = subst_env s tenv in
    let s1 = foo tenv exp (s b) in
    s1 @@ s
  | M.ASSIGN (e1, e2) ->
    let s1 = foo tenv e1 (TLoc typ) in
    let tenv = subst_env s1 tenv in
    let s2 = foo tenv e2 (s1 typ) in
    s2 @@ s1
  | M.BANG exp -> foo tenv exp (TLoc typ)
  | M.SEQ (e1, e2) ->
    let b = TVar (new_var ()) in
    let s1 = foo tenv e1 b in
    let tenv = subst_env s1 tenv in
    let s2 = foo tenv e2 (s1 typ) in
    s2 @@ s1
  | M.PAIR (eleft, eright) ->
    let bleft = TVar (new_var ()) in
    let bright = TVar (new_var ()) in
    let s = unify typ (TPair (bleft, bright)) in
    let tenv = subst_env s tenv in
    let s1 = foo tenv eleft (s bleft) in
    let s = s1 @@ s in
    let tenv = subst_env s tenv in
    let s1 = foo tenv eright (s bright) in (* bright?? *)
    s1 @@ s
  | M.FST exp ->
    let b2 = TVar (new_var ()) in
(*    let b2 = TVar (new_var ()) in
    let s = unify typ b1 in
    let tenv = subst_env s tenv in
    let s1 = foo tenv exp (TPair ((s b1), (s b2))) in
    s1 @@ s
*)
    let s1 = foo tenv exp (TPair (typ, b2)) in
    s1
  | M.SND exp ->
    let b1 = TVar (new_var ()) in
    let b2 = TVar (new_var ()) in
    let s = unify typ b2 in
    let tenv = subst_env s tenv in
    let s1 = foo tenv exp (TPair ((s b1), (s b2))) in
    s1 @@ s

let rec toMtyp (typ:typ) : M.typ =
  match typ with
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TPair (t1, t2) -> M.TyPair (toMtyp t1, toMtyp t2)
  | TLoc typ -> M.TyLoc (toMtyp typ)
  | TFun (t1, t2) -> raise (M.TypeError "final type is TFun")
  | TVar v -> raise (M.TypeError "final type is TVar")
  | Twrite
  | Tequal -> raise (M.TypeError "final type is Twrite/Tequal")

let check (exp:M.exp) : M.typ =
  let initial_type = TVar (new_var ()) in
  let s = foo [] exp initial_type in
  toMtyp (s initial_type)
