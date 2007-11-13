(* grand-pass.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This is an AST-to-AST pass that links together all the ast-opt passes. 
 *
 * This pass currently includes the following AST-to-AST translations:
 * - translation of parallel tuples into futures/touches
 * - rewriting of sumP into sumPQ
 *)

structure GrandPass : sig

    val transform : AST.module -> AST.module

  end = struct

    structure A = AST
    structure B = Basis
    structure F = Futures
    structure V = Var

    local

	val workQVar = V.new ("workQ", B.workQueueTy)
	val workQ    = A.VarExp (workQVar, [])
	val needsQ   = ref false

    in

    fun exp (A.LetExp (b, e)) = A.LetExp (binding b, exp e)
      | exp (A.IfExp (e1, e2, e3, t)) = A.IfExp (exp e1, exp e2, exp e3, t)
      | exp (A.CaseExp (e, ms, t)) = A.CaseExp (exp e, map match ms, t)
      | exp (A.HandleExp (e, ms, t)) = A.HandleExp (exp e, map match ms, t)
      | exp (A.RaiseExp (e, t)) = A.RaiseExp (exp e, t)
      | exp (A.FunExp (x, e, t)) = A.FunExp (x, exp e, t)
      | exp (A.ApplyExp (e1, e2, t)) = A.ApplyExp (exp e1, exp e2, t)
      | exp (A.TupleExp es) = A.TupleExp (map exp es)
      | exp (A.RangeExp (e1, e2, oe3, t)) = 
	  A.RangeExp (exp e1, exp e2, Option.map exp oe3, t)
      | exp (ptup as A.PTupleExp es) = 
	  (case trPTup es
	     of SOME e => (needsQ := true; e)
	      | NONE => A.TupleExp (map exp es)
	    (* end case *))
      | exp (A.PArrayExp (es, t)) = A.PArrayExp (map exp es, t)
      | exp (A.PCompExp (e, pes, oe)) = (needsQ := true; trPComp (e, pes, oe))
      | exp (A.PChoiceExp (es, t)) = A.PChoiceExp (map exp es, t)
      | exp (A.SpawnExp e) = A.SpawnExp (exp e)
      | exp (k as A.ConstExp _) = k
      | exp (v as A.VarExp (x, ts)) = 
	  (case trVar (x, ts)
	     of SOME e => (needsQ := true; e)
	      | NONE => v)
      | exp (A.SeqExp (e1, e2)) = A.SeqExp (exp e1, exp e2)
      | exp (x as A.OverloadExp _) = x

    and binding (A.ValBind (p, e)) = A.ValBind (p, exp e)
      | binding (A.PValBind (p, e)) = A.PValBind (p, exp e)
      | binding (A.FunBind lams) = A.FunBind (map lambda lams)

    and lambda (A.FB (f, x, e)) = A.FB (f, x, exp e)

    and match (A.PatMatch (p, e)) = A.PatMatch (p, exp e)
      | match (A.CondMatch (p, e1, e2)) = A.CondMatch (p, exp e1, exp e2)

    and trPTup arg = TranslatePtup.tr exp workQ arg

    and trVar arg = RewriteWithQueues.transform workQ arg

    and trPComp arg = TranslatePComp.tr exp workQ arg

  (* includeQ : A.module -> A.module *)
  (* Prepend module m with the appropriate let bindings to include the workQueue. *)
    fun includeQ m =
	A.LetExp (A.ValBind (A.VarPat workQVar, F.mkNewWorkQueue ()),
          A.LetExp (A.ValBind (A.WildPat B.workQueueTy, F.mkGetWork1All workQ),
            m))

  (* transform : A.module -> A.module *)
    fun transform m = 
	let val m' = exp m
            (* NOTE: Since exp is effectful, specifically in setting needsQ, *)
            (* it must be run before the following conditional is evaluated. *)
	in
	    (if !needsQ then includeQ else (fn x => x)) m'
	end

    end (* local *)

  end
