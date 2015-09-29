open CommonGrade
open Hw2_4

let empty = LOC (NODE [], TOP)
let loc = LOC (LEAF "*", HAND ([LEAF "c"], HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []), [LEAF "d"])) 
let loc1 = goLeft loc
let loc2 = goUp loc
let loc3 = goRight loc
let loc4 = goLeft (goLeft loc2)
let loc5 = goRight (goDown loc4)

let _ = check (fun () -> try goLeft empty = empty with NOMOVE _ -> true | _ -> false)
let _ = check (fun () -> try goRight empty = empty with NOMOVE _ -> true | _ -> false)
let _ = check (fun () -> try goUp empty = empty with NOMOVE _ -> true | _ -> false)
let _ = check (fun () -> try goDown empty = empty with NOMOVE _ -> true | _ -> false)

let _ = check (fun () -> try goDown loc = empty with NOMOVE _ -> true | _ -> false)

let _ = check (fun () -> loc1 = LOC (LEAF "c", HAND ([], HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []), [LEAF "*"; LEAF "d"])))
let _ = check (fun () -> try goLeft loc1 = empty with NOMOVE _ -> true | _ -> false)
let _ = check (fun () -> goRight loc1 = loc)
let _ = check (fun () -> try goDown loc1 = empty with NOMOVE _ -> true | _ -> false)

let _ = check (fun () -> loc2 = LOC (NODE [LEAF "c"; LEAF "*"; LEAF "d"], HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, [])))
let _ = check (fun () -> goLeft loc2 = LOC (LEAF "+", HAND ([NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, [NODE [LEAF "c"; LEAF "*"; LEAF "d"]])))
let _ = check (fun () -> goLeft (goLeft loc2) = LOC (NODE [LEAF "a"; LEAF "*"; LEAF "b"], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]])))
let _ = check (fun () -> try goLeft (goLeft (goLeft loc2)) = empty with NOMOVE _ -> true | _ -> false)
let _ = check (fun () -> try goRight loc2 = empty with NOMOVE _ -> true | _ -> false)
let _ = check (fun () -> goUp loc1 = loc2)
let _ = check (fun () -> goUp loc2 = LOC (NODE [NODE [LEAF "a"; LEAF "*"; LEAF "b"]; LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]], TOP))
let _ = check (fun () -> try goLeft (goUp loc2) = empty with NOMOVE _ -> true | _ -> false)
let _ = check (fun () -> try goRight (goUp loc2) = empty with NOMOVE _ -> true | _ -> false)
let _ = check (fun () -> try goUp (goUp loc2) = empty with NOMOVE _ -> true | _ -> false)
let _ = check (fun () -> goDown loc2 = loc1)

let _ = check (fun () -> loc3 = LOC (LEAF "d", HAND ([LEAF "*"; LEAF "c"], HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []), [])))
let _ = check (fun () -> goLeft loc3 = loc)
let _ = check (fun () -> try goRight loc3 = empty with NOMOVE _ -> true | _ -> false)
let _ = check (fun () -> goUp loc3 = loc2)
let _ = check (fun () -> try goDown loc3 = empty with NOMOVE _ -> true | _ -> false)

let _ = check (fun () -> loc4 = LOC (NODE [LEAF "a"; LEAF "*"; LEAF "b"], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]])))
let _ = check (fun () -> loc5 = LOC (LEAF "*", HAND ([LEAF "a"], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]), [LEAF "b"])))
