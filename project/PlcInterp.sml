(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun val2int (IntV n) = n
  | val2int _ = raise Impossible;

fun val2bool (BoolV b) = b
  | val2bool _ = raise Impossible;

fun val2list (SeqV l) = l
  | val2list (ListV l) = l
  | val2list _ = raise Impossible;

fun eval (e:expr) (p:plcVal env): plcVal =
  case e of
      (ConI n) => IntV n
    | (ConB b) => BoolV b
    | (ESeq _) => SeqV []
    | (Var x) => lookup p x

    | (Let (x, e1, e2)) => eval e2 ((x, (eval e1 p))::p)
    | (Letrec (f, t, x, t1, e1, e2)) =>
        let
          val closure = Clos (f, x, e1, p)
        in
          eval e2 ((f, closure)::p)
        end

    | (Prim1 ("!", e)) => let val v = eval e p in (BoolV (not (val2bool v))) end
    | (Prim1 ("-", e)) => let val v = eval e p in (IntV (~ (val2int v))) end
    | (Prim1 ("hd", e)) => let val v = eval e p in if val2list v = [] then raise HDEmptySeq else (hd (val2list v)) end
    | (Prim1 ("tl", e)) => let val v = eval e p in if val2list v = [] then raise TLEmptySeq else (SeqV (tl (val2list v))) end
    | (Prim1 ("ise", e)) => let val v = eval e p in if val2list v = [] then (BoolV true) else (BoolV false) end
    | (Prim1 ("print", e)) => let val v = print(val2string (eval e p)) in ListV [] end

    | (Prim2 ("&&", e1, e2)) => let val v1 = eval e1 p; val v2 = eval e2 p in BoolV (val2bool v1 andalso val2bool v2) end
    | (Prim2 ("+", e1, e2)) => let val v1 = eval e1 p; val v2 = eval e2 p in IntV (val2int v1 + val2int v2) end
    | (Prim2 ("-", e1, e2)) => let val v1 = eval e1 p; val v2 = eval e2 p in IntV (val2int v1 - val2int v2) end
    | (Prim2 ("*", e1, e2)) => let val v1 = eval e1 p; val v2 = eval e2 p in IntV (val2int v1 * val2int v2) end
    | (Prim2 ("/", e1, e2)) => let val v1 = eval e1 p; val v2 = eval e2 p in IntV (val2int v1 div val2int v2) end
    | (Prim2 ("=", e1, e2)) => let val v1 = eval e1 p; val v2 = eval e2 p in if v1 = v2 then BoolV true else BoolV false end
    | (Prim2 ("!=", e1, e2)) => let val v1 = eval e1 p; val v2 = eval e2 p in if v1 <> v2 then BoolV true else BoolV false end
    | (Prim2 ("<", e1, e2)) => let val v1 = eval e1 p; val v2 = eval e2 p in if val2int v1 < val2int v2 then BoolV true else BoolV false end
    | (Prim2 ("<=", e1, e2)) => let val v1 = eval e1 p; val v2 = eval e2 p in if val2int v1 <= val2int v2 then BoolV true else BoolV false end
    | (Prim2 ("::", e1, e2)) => let val v1 = eval e1 p; val v2 = eval e2 p in SeqV (v1::(val2list v2)) end
    | (Prim2 (";", e1, e2)) => let val v1 = eval e1 p; val v2 = eval e2 p in v2 end

    | (If (e, e1, e2)) =>
        let
          val b = eval e p
        in
          if (val2bool b) then eval e1 p else eval e2 p
        end

    | (Match (e, me)) =>
        let
          val v = eval e p
          fun findMatch [] = raise ValueNotFoundInMatch
            | findMatch (((SOME ei), ri)::t) = if v = eval ei p then eval ri p else findMatch t
            | findMatch ((NONE, ri)::t) = eval ri p
        in
          findMatch me
        end

    | (Call (e2, e1)) =>
        let val func = eval e2 p in
          case func of
            Clos(f, x, e, env) =>
              let
                val v = eval e1 p
                val env1 = ((x, v)::(f, func)::env)
              in
                eval e env1
              end
            | _ => raise NotAFunc
        end

    | (List []) => ListV []
    | (List l) => 
        let
          val list = map (fn x => eval x p) l
        in
          ListV list
        end

    | (Item (1, List (h::t))) => eval h p
    | (Item (i, List (h::t))) => eval (Item (i-1, List t)) p

    | (Anon ((t:plcType), (x:string), (e: expr))) => Clos ("", x, e, p)

    | _ => raise Impossible;