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

    fun transform _ = raise Fail "The function you have called, \
                                 \RewriteWithQueues.transform, \ 
                                 \has been disconnected."

(*
    structure A = AST
    structure B = Basis
    structure T = Types
    structure U = UnseenBasis
 		
  (* FIXME refactor this code... *)

  (* sumP : A.exp * A.ty list -> A.exp *)				  
    fun sumP ts = 
	let val t = TypeOf.exp (A.VarExp (B.sumP, ts))
	in
	   A.VarExp (U.sumP, [])
	end

  (* reduceP : A.exp * A.ty list => A.exp *)
  (* Consumes a work queue and a pair of types (in a length-two list) *)
  (*   representing the instantiating types in the reduction type. *)
    fun reduceP ts = 
      (case ts
	 of [alpha, beta] =>
	      let val funTy = TypeOf.exp (A.VarExp (B.reduceP, ts))
	      in
		  A.VarExp (U.reducePQ, [alpha, beta])
	      end
	  | _ => raise Fail "reduceP: expected two type args"
        (* end case *))

  (* transform : A.exp -> A.var * A.ty list -> A.exp option *)
    fun transform (x, ts) =
	  if Var.same (x, B.sumP) then 
	      SOME (sumP ts)
	  else if Var.same (x, B.reduceP) then
	      SOME (reduceP ts)
	  else 
	      NONE
	
*)

  end
