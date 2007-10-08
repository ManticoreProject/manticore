(* unpar.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * The function noPTups recursively turns parallel tuples into sequential tuples.
 *)

structure Unpar : sig

    val noPTups : AST.exp -> AST.exp

  end = struct

    structure A = AST

    fun exp (A.LetExp (b, e)) = A.LetExp (binding b, exp e)
      | exp (A.IfExp (e1, e2, e3, t)) = A.IfExp (exp e1, exp e2, exp e3, t)
      | exp (A.CaseExp (e, ms, t)) = A.CaseExp (exp e, map match ms, t)
      | exp (A.HandleExp (e, ms, t)) = A.HandleExp (exp e, map match ms, t)
      | exp (A.RaiseExp (e, t)) = A.RaiseExp (exp e, t)
      | exp (A.FunExp (x, e, t)) = A.FunExp (x, exp e, t)
      | exp (A.ApplyExp (e1, e2, t)) = A.ApplyExp (exp e1, exp e2, t)
      | exp (A.TupleExp es) = A.TupleExp (map exp es)
      | exp (A.RangeExp (e1, e2, oe3, t)) = A.RangeExp (exp e1, 
							exp e2,
							Option.map exp oe3,
							t)
      | exp (A.PTupleExp es) = A.TupleExp (map exp es)
      | exp (A.PArrayExp (es, t)) = A.PArrayExp (map exp es, t)
      | exp (A.PCompExp (e, pes, oe)) =
	  A.PCompExp (exp e,
			   map (fn (p,e) => (p, exp e)) pes,
			   Option.map exp oe)
      | exp (A.PChoiceExp (es, t)) = A.PChoiceExp (map exp es, t)
      | exp (A.SpawnExp e) = A.SpawnExp (exp e)
      | exp (k as A.ConstExp c) = k
      | exp (x as A.VarExp (v, ts)) = x
      | exp (A.SeqExp (e1, e2)) = A.SeqExp (exp e1, exp e2)
      | exp (A.OverloadExp ovr) = A.OverloadExp ovr

    and binding (A.ValBind (p, e)) = A.ValBind (p, exp e)
      | binding (A.PValBind (p, e)) = A.PValBind (p, exp e)
      | binding (A.FunBind lams) = A.FunBind (map lambda lams)

    and lambda (A.FB (f, x, e)) = A.FB (f, x, exp e)

    and match (A.PatMatch (p, e)) = A.PatMatch (p, exp e)
      | match (A.CondMatch (p, e1, e2)) = A.CondMatch (p, exp e1, exp e2)

    fun module m = exp m

    fun noPTups e = exp e

  end
