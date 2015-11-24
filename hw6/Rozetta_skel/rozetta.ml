(*
 * SNU 4190.310 Programming Languages
 * Homework "Rozetta" Skeleton
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

let trans_v : Sm5.value -> Sonata.value = function
  | Sm5.Z z  -> Sonata.Z z
  | Sm5.B b  -> Sonata.B b
  | Sm5.L _ -> raise (Sonata.Error "Invalid input program : pushing location")
  | Sm5.Unit -> Sonata.Unit
  | Sm5.R _ -> raise (Sonata.Error "Invalid input program : pushing record")

(* TODO : complete this function *)
let rec trans_obj : Sm5.obj -> Sonata.obj = function
  | Sm5.Val v -> Sonata.Val (trans_v v)
  | Sm5.Id id -> Sonata.Id id
  | Sm5.Fn (arg, command) ->
    let cmds =
    [
      Sonata.BIND "k#"
      (* stack에 있는 함수k 바인드 *)
    ]
    @ trans' (command @
    [
      Sm5.MALLOC;
      Sm5.BIND "tmp#";
      Sm5.PUSH (Sm5.Id "tmp#");
      Sm5.STORE;

      Sm5.MALLOC;
      Sm5.PUSH (Sm5.Id "tmp#");
      Sm5.LOAD;
      Sm5.PUSH (Sm5.Id "k#");
      Sm5.CALL
      (* call k *)
    ]) in
    Sonata.Fn (arg, cmds)

(* TODO : complete this function *)
and trans' : Sm5.command -> Sonata.command = function
  | Sm5.PUSH obj :: cmds -> Sonata.PUSH (trans_obj obj) :: (trans' cmds)
  | Sm5.POP :: cmds -> Sonata.POP :: (trans' cmds)
  | Sm5.STORE :: cmds -> Sonata.STORE :: (trans' cmds)
  | Sm5.LOAD :: cmds -> Sonata.LOAD :: (trans' cmds)
  | Sm5.JTR (c1, c2) :: cmds ->  Sonata.JTR (trans' (c1@cmds), trans' (c2@cmds))::[]
  | Sm5.MALLOC :: cmds -> Sonata.MALLOC :: (trans' cmds)
  | Sm5.BOX z :: cmds -> Sonata.BOX z :: (trans' cmds)
  | Sm5.UNBOX id :: cmds -> Sonata.UNBOX id :: (trans' cmds)
  | Sm5.BIND id :: cmds -> Sonata.BIND id :: (trans' cmds)
  | Sm5.UNBIND :: cmds -> Sonata.UNBIND :: (trans' cmds)
  | Sm5.GET ::cmds -> Sonata.GET :: (trans' cmds)
  | Sm5.PUT ::cmds -> Sonata.PUT :: (trans' cmds)
  | Sm5.CALL :: cmds ->
    [ Sonata.MALLOC;
      Sonata.BIND "l#";
      Sonata.PUSH (Sonata.Id "l#");
      Sonata.STORE;
      Sonata.MALLOC;
      Sonata.BIND "v#";
      Sonata.PUSH (Sonata.Id "v#");
      Sonata.STORE;
      Sonata.BIND "f#";

      Sonata.PUSH (trans_obj (Sm5.Fn ("x#", cmds)));

      Sonata.PUSH (Sonata.Id "f#");
      Sonata.PUSH (Sonata.Id "v#");
      Sonata.LOAD;
      Sonata.PUSH (Sonata.Id "l#");
      Sonata.LOAD;

      Sonata.CALL
      (*
       * 이미 쌓인 것 bind. store통해 제거.
       * 현재 상태 저장한 함수 k를 스택에 쌓아서 콜 된 함수 내부에서 바인드 할 수 있도록 함.
          Push trans_obj (Sm5.Fn (k, cmds))
       * 콜 하기 전에 제거했던 것 다시 복구.
       * 콜
       *
       * 이런 식으로 하면 trans는 어따가 써?
       * .........??스택은 왜 만들어?
       * 내가 왜 스택을 만들어야해?
       * 저런 식으로 하면 스택이 필요해?
       * 그냥 말록 바인드해서 메모리에 저장했다가 다시 쌓으면 안되나
       * 메모리 좀 쓴다고 안닳잖아.
       * 여기다가 그냥 쓰면 되잖아. 콜 안에서.
       * 여기다 그냥 쓰면 env가 바뀌어서 못씀..?
       * 근데 env좀 바뀌어도 뒤에서 쓰는 곳이 여기밖에 없으면 상관없는거 아닌가
       * 말록 하면 되잖아
       * 바인드도 하고
       * 왜ㅜㅜㅜ
       *
       * 안쓸거야!
       * 안써도 된다는거지? 이대로 할거면
       *)
    ]
  | Sm5.ADD :: cmds -> Sonata.ADD :: (trans' cmds)
  | Sm5.SUB :: cmds -> Sonata.SUB :: (trans' cmds)
  | Sm5.MUL :: cmds -> Sonata.MUL :: (trans' cmds)
  | Sm5.DIV :: cmds -> Sonata.DIV :: (trans' cmds)
  | Sm5.EQ :: cmds -> Sonata.EQ :: (trans' cmds)
  | Sm5.LESS :: cmds -> Sonata.LESS :: (trans' cmds)
  | Sm5.NOT :: cmds -> Sonata.NOT :: (trans' cmds)
  | [] -> []

(* TODO : complete this function *)
let trans : Sm5.command -> Sonata.command = trans'
