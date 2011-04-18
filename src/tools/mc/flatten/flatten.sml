(* flatten.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Glue the pieces of the flattening transformation together.
 *)

structure Flatten : sig

  val flatten : AST.exp -> AST.exp

end = struct

(* *** FLATTENING *** *)

  val verboseOutput = false

  val prAST = if verboseOutput then PrintAST.printExp 
	      else PrintAST.printExpNoTypesNoStamps

  fun flatten (e0 : AST.exp) : AST.exp = 
    if !FlattenControls.onFlg then let
      val _ = prAST e0
      val (e1, flatTycs) = FlattenTerms.flatten e0
      val e2 = FlattenOpFusion.fuseExp e1
      val e3 = RealizeTerms.realize (e2, flatTycs)
      val _ = print "********* flattened program:\n"
      val _ = prAST e3
      in
        e3
      end
    else e0

end
