(* rewrite-with-queues.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Some operations in Manticore require a work queue which is not immediately
 * visible in the surface program. This module provides a pass to rewrite 
 * those operations with a queue as needed.
 *)

structure RewriteWithQueues : sig

    val transform : AST.exp -> AST.var * AST.ty list -> AST.exp option

  end = struct

    structure A = AST
    structure B = Basis
    structure T = Types
    structure U = UnseenBasis
 		
    val changed = ref false
	 
  (* FIXME This obviously wants to be developed into a more general mechanism. *)

  (* isSumP : A.var -> bool *)
    fun isSumP x = Var.same (x, Basis.sumP)

  (* sumP : A.exp * A.ty -> A.exp *)				  
    fun sumP (q, t) = A.ApplyExp (A.VarExp (U.sumPQ, []), q, t)

  (* transform : A.exp -> A.var * A.ty list -> A.exp option *)
    fun transform q (x, ts) =
	  if isSumP x
	  then (changed := true; 
		SOME (sumP (q, TypeOf.exp (A.VarExp (x, ts)))))
	  else NONE
	
  end
