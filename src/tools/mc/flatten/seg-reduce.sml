(* seg-reduce.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Find terms that look like this:
 *  [| reduceUncurried (oper, ident, ns) | ns in nss |]
 * and turn them into this:
 *  PArray.segReduce (oper, ident, nss)
 *
 * Type-preserving translation.
 *)

structure SegReduce : sig

  val translate : AST.exp -> AST.exp

end = struct

  structure A = AST

  fun isReduceU e =
   (case e
      of A.VarExp (x, ts) => let
           val reduceUncurriedVar = DelayedBasis.Var.parrayReduce ()
           in
	     Var.same (x, reduceUncurriedVar)
	   end
       | _ => false)

  fun translate e = let
    fun exp (A.PCompExp (e, pes, optE)) = let
          val e' = translate e
	  val pes' = List.map (fn (p,e) => (p, translate e)) pes
	  val optE' = Option.map translate optE
          in case e'
	    of A.ApplyExp (f, args, t) =
                 if (isReduceU f) then
                   raise Fail "SegReduce.translate -- todo: FOUND ONE!!!!"
		 else
                   A.PCompExp (e', pes', optE')
	     | _ => A.PCompExp (e', pes', optE')
          end
      | exp e = raise Fail "translate: todo"
    in
      exp e
    end

end
