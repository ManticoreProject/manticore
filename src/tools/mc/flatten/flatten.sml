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

  val segreduce = BasicControl.mkKeepPassSimple {
    output = PrintAST.outputExpNoTypes, 
    ext = "ast",
    passName = "segr",
    pass = SegReduce.translate,
    registry = FlattenControls.registry
  }

 fun fltExp (e0 : AST.exp) : AST.exp = 
    if not (!FlattenControls.onFlg) then e0
    else let
      val e1 = segreduce e0
      val (e2, flatTycs) = FlattenTerms.flatten e1
      val e3 = FlattenOpFusion.fuseExp e2
      val e4 = RealizeTerms.realize (e3, flatTycs)
      in
        e4
      end

 val flatten = BasicControl.mkKeepPassSimple {
    output = PrintAST.outputExp, (* NoTypes, *)
    ext = "ast",
    passName = "ft",
    pass = fltExp,
    registry = FlattenControls.registry
  }

end
