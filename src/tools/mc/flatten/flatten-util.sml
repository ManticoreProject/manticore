(* flatten-util.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure FlattenUtil = struct

  structure A = AST
  structure B = Basis
  structure T = Types

  structure AU = ASTUtil
  structure TU = TypeUtil

  structure D  = DelayedBasis
  structure DC = D.TyCon
  structure DD = D.DataCon
  structure DV = D.Var

(* currying operator *)
  fun `f x y = f (x, y)

(* isGroundTy : T.ty -> bool *)
  local
    fun isg c = List.exists (`TyCon.same c) B.primTycs
  in
    fun isGroundTy (T.ConTy ([], c)) = isg c
      | isGroundTy _ = false
  end (* local *)

(* isFArrayTyc : T.tycon -> bool *)
(*  val isFArrayTyc : T.tycon -> bool = `TyCon.same (DC.farray ()) *)
  fun isFArrayTyc c = TyCon.same (c, DC.farray ())

(* isInt : T.ty -> bool *)
  val isInt = `TU.same B.intTy

(* isLf : A.ntree -> bool *)
  val isLf = (fn A.Lf _ => true | _ => false)

(* ***** debugging misc ***** *)

(* debugging utilities *)
  fun expressionForm (x : A.exp) : string = let
    fun e (A.LetExp _) = "LetExp"
      | e (A.IfExp _) = "IfExp"
      | e (A.CaseExp _) = "CaseExp"
      | e (A.PCaseExp _) = "PCaseExp"
      | e (A.HandleExp _) = "HandleExp"
      | e (A.RaiseExp _) = "RaiseExp"
      | e (A.FunExp _) = "FunExp"
      | e (A.ApplyExp _) = "ApplyExp"
      | e (A.VarArityOpExp _) = "VarArityOpExp"
      | e (A.TupleExp _) = "TupleExp"
      | e (A.RangeExp _) = "RangeExp"
      | e (A.PTupleExp _) = "PTupleExp"
      | e (A.PArrayExp _) = "PArrayExp"
      | e (A.PCompExp _) = "PCompExp"
      | e (A.PChoiceExp _) = "PChoiceExp"
      | e (A.SpawnExp _) = "SpawnExp"
      | e (A.ConstExp _) = "ConstExp"
      | e (A.VarExp _) = "VarExp"
      | e (A.SeqExp _) = "SeqExp"
      | e (A.OverloadExp _) = "OverloadExp"
      | e (A.ExpansionOptsExp _) = "ExpansionOptsExp"
      | e (A.FTupleExp _) = "FTupleExp"
      | e (A.FArrayExp _) = "FArrayExp"
      | e (A.FlOp _) = "FlOp"
      | e (A.PArrayOp _) = "PArrayOp"
    in
      e x
    end

  fun look (e : A.exp) : unit = let
    fun ln () = print "\n"
    fun pr s = (print s; ln ())
    fun exp (A.LetExp (b, e)) = (pr ("let " ^ binding b ^ " in"); exp e)
      | exp (A.ExpansionOptsExp (opts, e)) = (pr "expansion"; exp e)
      | exp (A.TupleExp []) = pr "()"
      | exp (A.TupleExp es) = (pr "tuple("; app exp es; pr ")")
      | exp e = pr (expressionForm e) 
    and binding (A.ValBind (p, e)) = pat p ^ " = ..."
      | binding (A.PValBind (p, e)) = pat p ^ " = ..."
      | binding (A.FunBind lams) = String.concatWith "\n" (map lam lams)
      | binding (A.PrimVBind (x, _)) = var x ^ " = _prim(...)"
      | binding (A.PrimCodeBind c) = "_primcode(...)"
    and var x = Var.toString x
    and pat (A.ConPat (c, _, p)) = DataCon.toString c ^ "(" ^ pat p ^ ")"
      | pat (A.TuplePat ps) = "(" ^ String.concatWith "," (map pat ps) ^ ")"
      | pat (A.VarPat x) = var x
      | pat (A.WildPat _) = "_"
      | pat (A.ConstPat c) = "<const>"
    and lam (A.FB (f, x, b)) = "fun " ^ var f ^ "(" ^ var x ^ ") = ..."
    in
      pr "^^^^^ running LOOK"; exp e; raise Fail "HALT"
    end

end
