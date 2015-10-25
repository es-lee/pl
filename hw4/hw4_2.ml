exception IMPOSSIBLE
exception Error of string
type treasure = StarBox | NameBox of string
type key = Bar | Node of key * key
type keyvar = Barvar
            | Nodevar of keyvar * keyvar
            | Unknown of int
type map = End of treasure
         | Branch of map * map
         | Guide of string * map

let print_int_endline i =
  let _ = print_int i in
  print_string "\n"

let rec foo m id eqlist boxlist nboxlist nextid =
  match m with
  | End StarBox ->
      (id, (Barvar, Unknown id)::eqlist, (Unknown id)::boxlist, nboxlist, nextid)
  | End (NameBox x) ->
      if (List.exists (fun (a, b) -> a = x) nboxlist) then
        let (_, bar) = List.find (fun (a, b) -> a = x) nboxlist in
        (id, (Unknown id, bar)::eqlist, boxlist, nboxlist, nextid)
      else (id, eqlist, (Unknown id)::boxlist, (x, Unknown id)::nboxlist, nextid)
  | Guide (s, m') ->
      let (a1, eqlist, boxlist, nboxlist, nextid) = (foo m' nextid eqlist boxlist nboxlist (nextid + 1)) in
      let (_, b1) = List.find (fun (a, b) -> a = s) nboxlist in
      (id, (Unknown id, Nodevar (b1,Unknown a1))::eqlist, boxlist, nboxlist, nextid)
  | Branch (m1, m2) ->
      let (a1, elist, blist, nlist, nextid) = (foo m1 nextid eqlist boxlist nboxlist (nextid + 1)) in
      let (b1, elist, blist, nlist, nextid) = (foo m2 nextid elist blist nlist (nextid + 1)) in
      (id, (Unknown a1, Nodevar (Unknown b1, Unknown id))::elist, blist, nlist, nextid)

let subst x y =
  let rec s t =
    match t with
    | Unknown i -> if t = x then y else t
    | Nodevar (v1, v2) -> Nodevar (s v1, s v2)
    | Barvar -> Barvar
  in s

let identity = (fun x -> x)
let ( @ ) s1 s0 = (fun (x) -> s1 (s0 x))
let rec inside v1 v2 =
  match (v1, v2) with
  | (Unknown a, Unknown b) ->
      if a = b then true else false
  | (Unknown a, Nodevar (x1, x2)) ->
      let x = inside v1 x1 in
      let y = inside v1 x2 in
      (x || y)
  | _ -> false

let rec unify v1 v2 =
  match (v1, v2) with
  | (Unknown x, _) ->
      if v1 = v2 then identity
      else if not (inside v1 v2) then subst v1 v2
      else raise IMPOSSIBLE
  | (_, Unknown x) -> unify v2 v1
  | (Nodevar (v1, v2), Nodevar (v3, v4)) ->
      let s0 = unify v1 v3 in
      let s1 = unify (s0 v2) (s0 v4) in
      s1@s0
  | _ ->
      if v1 = v2 then identity
      else raise IMPOSSIBLE

let rec unifyall s eqlist =
  match eqlist with
  | [] -> s
  | (a, b)::t -> unifyall ((unify (s a) (s b))@s) t

let reactall s lst =
  let rec react v1 =
    match v1 with
    | Barvar -> Barvar
    | Unknown a ->
        let v2 = s v1 in
        if (v1 = v2) then Barvar
        else react v2
    | Nodevar (a, b) -> Nodevar (react a, react b) in
  List.map react lst

let rec keyvarToKey keyv =
  match keyv with
  | Barvar -> Bar
  | Nodevar (v1, v2) -> Node (keyvarToKey v1, keyvarToKey v2)
  | Unknown i -> raise (Error "unknown exist")

let rec dedup lst =
  match lst with
  | [] -> lst
  | h::t ->
      let lst = List.filter (fun x -> x <> h) t in
      h::(dedup lst)

let getReady m =
  let _, eqlist, boxlist, _, _ = foo m 0 [] [] [] 1 in
  let s = unifyall identity eqlist in
  let result = reactall s boxlist in
  let dup = List.map keyvarToKey result in
  dedup dup
