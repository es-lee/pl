(*
 * SNU 4190.310 Programming Languages 2015 Fall
 *  K- Interpreter Skeleton Code
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

(* Location Signature *)
module type LOC =
sig
  type t
  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
end

module Loc : LOC =
struct
  type t = Location of int
  let base = Location(0)
  let equal (Location(a)) (Location(b)) = (a = b)
  let diff (Location(a)) (Location(b)) = a - b
  let increase (Location(base)) n = Location(base+n)
end

(* Memory Signature *)
module type MEM =
sig
  type 'a t
  exception Not_allocated
  exception Not_initialized
  val empty : 'a t (* get empty memory *)
  val load : 'a t -> Loc.t  -> 'a (* load value : Mem.load mem loc => value *)
  val store : 'a t -> Loc.t -> 'a -> 'a t (* save value : Mem.store mem loc value => mem' *)
  val alloc : 'a t -> Loc.t * 'a t (* get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(* Environment Signature *)
module type ENV =
sig
  type ('a, 'b) t
  exception Not_bound
  val empty : ('a, 'b) t (* get empty environment *)
  val lookup : ('a, 'b) t -> 'a -> 'b (* lookup environment : Env.lookup env key => content *)
  val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t  (* id binding : Env.bind env key content => env'*)
end

(* Memory Implementation *)
module Mem : MEM =
struct
  exception Not_allocated
  exception Not_initialized
  type 'a content = V of 'a | U
  type 'a t = M of Loc.t * 'a content list
  let empty = M (Loc.base,[])

  let rec replace_nth = fun l n c ->
    match l with
    | h::t -> if n = 1 then c :: t else h :: (replace_nth t (n - 1) c)
    | [] -> raise Not_allocated

  let load (M (boundary,storage)) loc =
    match (List.nth storage ((Loc.diff boundary loc) - 1)) with
    | V v -> v
    | U -> raise Not_initialized

  let store (M (boundary,storage)) loc content =
    M (boundary, replace_nth storage (Loc.diff boundary loc) (V content))

  let alloc (M (boundary,storage)) =
    (boundary, M (Loc.increase boundary 1, U :: storage))
end

(* Environment Implementation *)
module Env : ENV=
struct
  exception Not_bound
  type ('a, 'b) t = E of ('a -> 'b)
  let empty = E (fun x -> raise Not_bound)
  let lookup (E (env)) id = env id
  let bind (E (env)) id loc = E (fun x -> if x = id then loc else env x)
end

(*
 * K- Interpreter
 *)
module type KMINUS =
sig
  exception Error of string
  type id = string
  type exp =
  | NUM of int | TRUE | FALSE | UNIT
  | VAR of id
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | SEQ of exp * exp            (* sequence *)
  | IF of exp * exp * exp       (* if-then-else *)
  | WHILE of exp * exp          (* while loop *)
  | LETV of id * exp * exp      (* variable binding *)
  | LETF of id * id list * exp * exp (* procedure binding *)
  | CALLV of id * exp list      (* call by value *)
  | CALLR of id * id list       (* call by referenece *)
  | RECORD of (id * exp) list   (* record construction *)
  | FIELD of exp * id           (* access record field *)
  | ASSIGN of id * exp          (* assgin to variable *)
  | ASSIGNF of exp * id * exp   (* assign to record field *)
  | READ of id
  | WRITE of exp

  type program = exp
  type memory
  type env
  type value =
  | Num of int
  | Bool of bool
  | Unit
  | Record of (id -> Loc.t)
  val emptyMemory : memory
  val emptyEnv : env
  val run : memory * env * program -> value
end

module K : KMINUS =
struct
  exception Error of string

  type id = string
  type exp =
  | NUM of int | TRUE | FALSE | UNIT
  | VAR of id
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | SEQ of exp * exp            (* sequence *)
  | IF of exp * exp * exp       (* if-then-else *)
  | WHILE of exp * exp          (* while loop *)
  | LETV of id * exp * exp      (* variable binding *)
  | LETF of id * id list * exp * exp (* procedure binding *)
  | CALLV of id * exp list      (* call by value *)
  | CALLR of id * id list       (* call by referenece *)
  | RECORD of (id * exp) list   (* record construction *)
  | FIELD of exp * id           (* access record field *)
  | ASSIGN of id * exp          (* assgin to variable *)
  | ASSIGNF of exp * id * exp   (* assign to record field *)
  | READ of id
  | WRITE of exp

  type program = exp

  type value =
  | Num of int
  | Bool of bool
  | Unit
  | Record of (id -> Loc.t)

  type memory = value Mem.t
  type env = (id, env_entry) Env.t
  and  env_entry = Addr of Loc.t | Proc of id list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

  let value_int v =
    match v with
    | Num n -> n
    | _ -> raise (Error "TypeError : not int")

  let value_bool v =
    match v with
    | Bool b -> b
    | _ -> raise (Error "TypeError : not bool")

  let value_unit v =
      match v with
      | Unit -> ()
      | _ -> raise (Error "TypeError : not unit")

  let value_record v =
      match v with
      | Record r -> r
      | _ -> raise (Error "TypeError : not record")

  let lookup_env_loc e x =
    try
      (match Env.lookup e x with
      | Addr l -> l
      | Proc _ -> raise (Error "TypeError : not addr"))
    with Env.Not_bound -> raise (Error "Unbound")

  let lookup_env_proc e f =
    try
      (match Env.lookup e f with
      | Addr _ -> raise (Error "TypeError : not proc")
      | Proc (id_list, exp, env) -> (id_list, exp, env))
    with Env.Not_bound -> raise (Error "Unbound")

  let rec eval mem env e =
    match e with
    | NUM v -> (Num v, mem)
    | TRUE -> (Bool true, mem)
    | FALSE -> (Bool false, mem)
    | UNIT -> (Unit, mem)
    | VAR x ->
      let l = lookup_env_loc env x in
      (Mem.load mem l, mem)
    | ADD (e1, e2) ->
      let (v1, m1) = eval mem env e1 in
      let n1 = value_int v1 in
      let (v2, m2) = eval m1 env e2 in
      let n2 = value_int v2 in
      (Num (n1 + n2), m2)
    | SUB (e1, e2) ->
      let (v1, m1) = eval mem env e1 in
      let n1 = value_int v1 in
      let (v2, m2) = eval m1 env e2 in
      let n2 = value_int v2 in
      (Num (n1 - n2), m2)
    | MUL (e1, e2) ->
      let (v1, m1) = eval mem env e1 in
      let n1 = value_int v1 in
      let (v2, m2) = eval m1 env e2 in
      let n2 = value_int v2 in
      (Num (n1 * n2), m2)
    | DIV (e1, e2) ->
      let (v1, m1) = eval mem env e1 in
      let n1 = value_int v1 in
      let (v2, m2) = eval m1 env e2 in
      let n2 = value_int v2 in
      (Num (n1 / n2), m2)
    | EQUAL (e1, e2) ->
      let (v1, m1) = eval mem env e1 in
      let (v2, m2) = eval m1 env e2 in
      (Bool (v1 = v2), m2)
    | LESS (e1, e2) ->
      let (v1, m1) = eval mem env e1 in
      let n1 = value_int v1 in
      let (v2, m2) = eval m1 env e2 in
      let n2 = value_int v2 in
      (Bool (n1 < n2), m2)
    | NOT e ->
      let (v, m') = eval mem env e in
      let b = value_bool v in
      (Bool (not b), m')
    | SEQ (e1, e2) ->
      let (v, m1) = eval mem env e1 in
      eval m1 env e2
    | IF (e, e1, e2) ->
      let (v, m) = eval mem env e in
      let b = value_bool v in
      if b then (eval m env e1) else (eval m env e2)
    | WHILE (e1, e2) ->
      let (v, m) = eval mem env e1 in
      let b = value_bool v in
      if b then (let (v', m') = eval m env e2 in
                 (eval m' env (WHILE (e1, e2)))
                ) else (Unit, m)
    | LETV (x, e1, e2) ->
      let (v, mem') = eval mem env e1 in
      let (l, mem'') = Mem.alloc mem' in
      eval (Mem.store mem'' l v) (Env.bind env x (Addr l)) e2
    | LETF (x, xl, e1, e2) ->
      let env' = Env.bind env f (Proc (xl, e1, env)) in
      eval mem env' e2
    | CALLV (x, el) ->
      let vl = [] in
      let (vl_r, mem') =
        List.fold_left (fun (vl, mem) e ->
                        let (v, mem') = eval mem env e in
                        (v::vl, mem')) (vl, mem) el in
      let vl = List.rev vl_r in
      let (id_lst, e', env') = lookup_env_proc env x in
      let env'' =
        Env.bind (List.fold_left (fun (env, mem) id ->
                        let (l, m) = Mem.alloc mem in
                        (Env.bind env id (Addr l), m)) (env', mem') id_lst)
                 x (Proc (id_lst, e', env')) in
      if (List.length vl) = (List.length id_lst) then
        let mem'' = (List.fold_left2 (fun mem x v ->
                         let l = lookup_env_loc env'' x in
                         Mem.store mem l v) mem' id_lst vl) in
        eval mem'' env'' e'
      else raise (Error "InvalidArg")
      (*List.nth lst n*)
    | CALLR (f, yl) ->
      let (xl, e, env') = lookup_env_proc env f in
      let env'' = Env.bind (List.fold_left2 (fun env' x y ->
                    let l = lookup_env_proc env y in
                    Env.bind env' x l) env' xl yl)
                    f (Proc (xl, e, env')) in
      eval mem env'' e
    | RECORD [] -> (Unit, mem)
    | RECORD rl ->
      let vl = [] in
      let (vl_r, mem') =
        List.fold_left (fun (vl, mem) (id, e) ->
                        let (v, mem') = eval mem env e in
                        (v::vl, mem')) (vl, mem) rl in
      let vl = List.rev vl_r in
      let (recenv, mem_r) = (List.fold_left
                        (fun (env, mem) (id, e) ->
                        let (l, m) = Mem.alloc mem in
                        (Env.bind env id (Addr l), m))
                        (Env.empty, mem') id_lst) in
      let mem_r = List.fold_left2 (fun mem (id, e) v ->
                          let l = lookup_env_loc recenv id in
                          Mem.store mem l v) mem_r rl vl in
      (Record (fun (x) -> (lookup_env_loc recenv) x), mem_r)
    | FIELD (e, x) ->
      let (r, m') = eval mem env e in
      let record = value_record r in
      (Mem.load m' (record x), m')
    | ASSIGN (x, e) ->
      let (v, mem') = eval mem env e in
      let l = lookup_env_loc env x in
      (v, Mem.store mem' l v)
    | ASSIGNF (e1, x, e2) ->
      let (r, m1) = eval mem env e1 in
      let (v, m2) = eval m1 env e2 in
      let record = value_record r in
      let l = record x in
      (v, Mem.store m2 l v)
    | READ x ->
      let v = Num (read_int()) in
      let l = lookup_env_loc env x in
      (v, Mem.store mem l v)
    | WRITE e ->
      let (v, mem') = eval mem env e in
      let n = value_int v in
      let _ = print_endline (string_of_int n) in
      (v, mem')
    | _ -> failwith "Unimplemented" (* TODO : Implement rest of the cases *)

  let run (mem, env, pgm) =
    let (v, _ ) = eval mem env pgm in
    v
end
