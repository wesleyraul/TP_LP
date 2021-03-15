(* PlcChecker *)

exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList

use "Environ.sml";
use "Absyn.sml";

fun isSeqType (SeqT t: plcType) = true
      | isSeqType _ = false;

fun isEqType IntT = true
      | isEqType BoolT = true
      | isEqType (ListT []) = true
      | isEqType (ListT (h::[])) = isEqType h
      | isEqType (ListT (h::t)) = if isEqType h then isEqType (ListT t) else false
      | isEqType (SeqT (t)) = isEqType t
      | isEqType _ = false;

fun teval (e:expr) (p:plcType env): plcType =
  case e of
      (Var x) => lookup p x (* type(x,p) *)
    | (ConI _) => IntT (* type(n,p) *)
    | (ConB _) => BoolT (* type(false/true, p) *)
    | (List []) => ListT [] (* type((),p) *)

    | (List l) => (* type(e1,...,en) *) (*ERRO*)
    let
      val list = map (fn t => teval t p) l 
    in
      ListT list
    end
    | (ESeq (SeqT t)) => SeqT t (* type((t []), p)*)
    | (ESeq _) => raise EmptySeq
    | (Let ((x:string), (e1:expr), (e2:expr))) => (* type(var x = e1; e2, p) *)
        let
          val t1 = teval e1 p
        in
          teval e2 ((x, t1) :: p)
        end
    | (Letrec (f,t,x,t1,e1,e2)) =>(*ERRO*)
    (* Letrec of string * plcType * string * plcType * expr * expr *)
        let
          val auxt1 = teval e1 ((f, (FunT(t, t1)))::(x, t)::p)
        in
          if t1 = auxt1 then teval e2 ((f, (FunT(t, t1)))::p) else raise UnknownType
        end
    | Anon(t:plcType, x:string, e:expr) => FunT(t, teval e ((x, t):: p)) (* type(fn (s x) => e end, ρ) *)
    | Call(e2, e1) =>
        let
          fun aux (FunT(s, t)) = t
            | aux _ = raise NotFunc
          val t1 = teval e1 p
          val t2 = aux(teval e2 p)
        in
          if teval e2 p = FunT(t1, t2) then t2 else raise CallTypeMisM
        end
    | If(e, e1, e2) =>
        let
          val te = teval e p
          val te1 = teval e1 p
          val te2 = teval e2 p
        in
          if te <> BoolT then raise IfCondNotBool else if te1 = te2 then te1 else raise DiffBrTypes
        end
    | Match(e, []) => raise NoMatchResults
    | Match(e, ((ei, ri)::[])) => IntT
        (* let
          val te = teval e p
          val tei = teval (getOpt (ei, List [])) p
          val tri = teval ri p
        in
          if tei <> ListT [] andalso te = tei andalso tri 
        end *)
    | Match(e, ((ei, ri)::t)) => IntT
        (* let
          val te = teval e p
          val tei = teval (getOpt (ei, List [])) p
          val tri = teval ri p
        in
          if tei <> ListT [] andalso te = tei andalso tri 
        end *)
    | Prim1("!", e) => if teval e p = BoolT then BoolT else raise UnknownType
    | Prim1("-", e) => if teval e p = IntT then IntT else raise UnknownType
    | Prim1("hd", e) =>
        let
          val t = teval e p
        in
          if isSeqType t then t else raise UnknownType
        end
    | Prim1("tl", e) =>
        let
          val t = teval e p
        in
          if isSeqType t then SeqT t else raise UnknownType
        end
    | Prim1("ise", e) =>
        let
          val t = teval e p
        in
          if isSeqType t then BoolT else raise UnknownType
        end
    | Prim1("print", e) => if teval e p = teval e p then ListT [] else raise UnknownType
    | Prim2("&&", e1, e2) =>
        let
          val te1 = teval e1 p
          val te2 = teval e2 p
        in
          if te1 = BoolT andalso te2 = BoolT then BoolT else raise UnknownType
        end
    | Prim2("+", e1, e2) =>
        let
          val te1 = teval e1 p
          val te2 = teval e2 p
        in
          if te1 = IntT andalso te2 = IntT then IntT else raise UnknownType
        end
    | Prim2("-", e1, e2) =>
        let
          val te1 = teval e1 p
          val te2 = teval e2 p
        in
          if te1 = IntT andalso te2 = IntT then IntT else raise UnknownType
        end
    | Prim2("*", e1, e2) =>
        let
          val te1 = teval e1 p
          val te2 = teval e2 p
        in
          if te1 = IntT andalso te2 = IntT then IntT else raise UnknownType
        end
    | Prim2("/", e1, e2) =>
        let
          val te1 = teval e1 p
          val te2 = teval e2 p
        in
          if te1 = IntT andalso te2 = IntT then IntT else raise UnknownType
        end
    | Prim2("<", e1, e2) =>
        let
          val te1 = teval e1 p
          val te2 = teval e2 p
        in
          if te1 = IntT andalso te2 = IntT then BoolT else raise UnknownType
        end
    | Prim2("<=", e1, e2) =>
        let
          val te1 = teval e1 p
          val te2 = teval e2 p
        in
          if te1 = IntT andalso te2 = IntT then BoolT else raise UnknownType
        end
    | Prim2("=", e1, e2) =>
        let
          val te1 = teval e1 p
          val te2 = teval e2 p
        in
          if te1 = te2 andalso isEqType te1 then BoolT else raise UnknownType
        end
    | Prim2("!=", e1, e2) =>
        let
          val te1 = teval e1 p
          val te2 = teval e2 p
        in
          if te1 = te2 andalso isEqType te1 then BoolT else raise UnknownType
        end
    | Item (0, List (h::t)) => teval h p
    | Item (i, List []) => raise ListOutOfRange
    | Item (i, List (h::t)) => teval (Item (i-1, (List t))) p
    | _ => raise UnknownType;