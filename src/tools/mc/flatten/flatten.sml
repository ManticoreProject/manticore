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

  fun flatten' (e0 : AST.exp) : AST.exp = 
    if not (!FlattenControls.onFlg) then e0
    else let
      val (e1, flatTycs) = FlattenTerms.flatten e0
      val e2 = FlattenOpFusion.fuseExp e1
      val e3 = RealizeTerms.realize (e2, flatTycs)
      in
        e3
      end

  val flatten = BasicControl.mkKeepPassSimple {
    output = PrintAST.outputExp,
    ext = "ast",
    passName = "ft",
    pass = flatten',
    registry = FlattenControls.registry
  }

end
