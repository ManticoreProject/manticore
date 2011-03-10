(* test-flatten-types.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure TestFlattenTypes = struct

  structure T = Types
  structure B = Basis
  structure U = TypeUtil
  
  val env = FlattenEnv.mkEnv ()

  val println = (fn s => (print s; print "\n"))

  val unitTy = B.unitTy

  fun mkTest t = (fn () => let
    val r = FlattenTypes.flattenTy env t
    in
      println ("t:  " ^ TypeUtil.toString t);
      println ("r:  " ^ TypeUtil.toString r)
    end)

  infixr **
  fun t1 ** t2 = T.TupleTy [t1, t2]

  infixr -->
  fun t1 --> t2 = T.FunTy (t1, t2)

  val test0 = mkTest unitTy

  val test1 = mkTest (unitTy ** unitTy)

  val test2 = mkTest (B.parrayTy unitTy)

  val test3 = mkTest (B.parrayTy (unitTy ** unitTy))

  val test4 = mkTest (B.parrayTy (B.parrayTy unitTy))

  val test5 = mkTest (B.parrayTy (B.parrayTy (unitTy ** unitTy)))

  val test6 = mkTest (unitTy --> unitTy)

  val test7 = mkTest ((B.parrayTy unitTy) --> unitTy)

  val test8 = mkTest (B.parrayTy (unitTy --> unitTy))

  val test9 = mkTest (B.parrayTy (unitTy --> (B.parrayTy unitTy)))

(* so far, so good *)

(* let's concoct a datatype for experiments... *)
(* datatype a = A of int parray | B of bool parray parray *)
  val aTyc = TyCon.newDataTyc (Atom.atom "a", [])
  val dconA = DataCon.new aTyc (Atom.atom "A", SOME (B.parrayTy B.intTy))
  val dconB = DataCon.new aTyc (Atom.atom "B", SOME (B.parrayTy (B.parrayTy B.boolTy)))

  val test99 = mkTest (T.ConTy ([], aTyc))

  fun foo () =
    (case FlattenEnv.findDCon (env, dconA)
      of SOME c => print (DataCon.toString c ^ "\n")
       | NONE => print "NONE\n")

end
