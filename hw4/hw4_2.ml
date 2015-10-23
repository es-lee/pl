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
      let _ = print_int_endline id in
      if (List.exists (fun (a, b) -> a = x) nboxlist) then
        let (_, bar) = List.find (fun (a, b) -> a = x) nboxlist in
        (id, (bar ,Unknown id)::eqlist, boxlist, nboxlist, nextid)
      else (id, eqlist, (Unknown id)::boxlist, (x, Unknown id)::nboxlist, nextid)
  | Guide (s, m') ->
      let (a1, eqlist, boxlist, nboxlist, nextid) = (foo m' nextid eqlist boxlist nboxlist (nextid + 1)) in
      let (_, b1) = List.find (fun (a, b) -> a = s) nboxlist in
      (id, (Unknown id, Nodevar (Unknown a1, b1))::eqlist, boxlist, nboxlist, nextid)
  | Branch (m1, m2) ->
      let (a1, elist, blist, nlist, nextid) = (foo m1 nextid eqlist boxlist nboxlist (nextid + 1)) in
      let (b1, elist, blist, nlist, nextid) = (foo m2 nextid elist blist nlist (nextid + 1)) in
      (id, (Unknown a1, Nodevar (Unknown b1, Unknown id))::elist, blist, nlist, nextid)

let _, eqlist, boxlist, _, _ = foo (Branch (Guide ("x", End (NameBox "x")),End StarBox)) 0 [] [] [] 1

let getReady m = print_endline ("hahaha")
