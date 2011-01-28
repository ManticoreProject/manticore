(* test-flatten-types.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure TestFlattenTypes = struct

  structure T = Types
  structure I = InterfaceTypes
  structure N = NestingTreeTypes
  structure R = RepresentationTypes
  structure F = FTTypes
  structure U = FTTypeUtil
  
  val println = (fn s => (print s; print "\n"))

  fun test0 () = let
    val t = Basis.intTy
    val t' = TranslateTypesFT.ty t
    in
      (I.toString t', 
       R.toString (FlattenTypes.flatten t'))
    end

  fun test1 () = let
    val t = T.ConTy ([Basis.intTy], Basis.parrayTyc)
    val _ = println ("t: " ^ TypeUtil.toString t)
    val t' = TranslateTypesFT.translate t
    val _ = println ("t': " ^ I.toString t')
    in
      (I.toString t', 
       R.toString (FlattenTypes.flatten t'))
    end

end
