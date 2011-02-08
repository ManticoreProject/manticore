(* test-flatten-types.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure TestFlattenTypes = struct

  structure T = Types
  structure B = Basis
  structure I = InterfaceTypes
  structure N = NestingTreeTypes
  structure R = RepresentationTypes
  structure F = FTTypes
  structure U = FTTypeUtil
  
  val println = (fn s => (print s; print "\n"))

  fun mkTest t = (fn () => let
    val t' = TranslateTypesFT.ty t
    val f = FlattenTypes.flatten t'
    in
      (println ("t:  " ^ TypeUtil.toString t);
       println ("t': " ^ I.toString t');
       println ("f:  " ^ R.toString f);
       println ("result is flat: " ^ Bool.toString (U.isFlat f)))
    end)

  val test0 = mkTest B.intTy

  infixr **
  fun t1 ** t2 = T.TupleTy [t1, t2]

  val intParr = B.parrayTy B.intTy

  val test1 = mkTest intParr

  val test2 = mkTest (B.parrayTy intParr)

  val test3 = mkTest (B.intTy ** B.intTy)

  val test4 = mkTest (B.parrayTy (B.intTy ** B.intTy))

  val test5 = mkTest (B.parrayTy ((B.parrayTy B.intTy) ** B.intTy))

  val test6 = mkTest (B.parrayTy ((B.parrayTy B.intTy) ** (B.parrayTy B.intTy)))

  val test7 = mkTest (B.parrayTy B.intTy ** (B.parrayTy B.intTy ** B.parrayTy B.intTy))

end
