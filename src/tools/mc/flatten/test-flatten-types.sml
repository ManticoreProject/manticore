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

  fun dt (tyc as T.Tyc {def, ...}) = (case def
    of T.AbsTyc => TyCon.toString tyc
     | T.DataTyc {cons, ...} => let
         val cs = !cons
         val ss = List.map DataCon.toString cs
         in
           String.concat [TyCon.toString tyc, 
			  "[",
			  String.concatWith "," ss,
			  "]"]
         end)

  fun toStr (t as T.ConTy (_, c)) = dt c
    | toStr t = U.toString t

  fun mkTest t = (fn () => let
    val r = FlattenTypes.flattenTy env t
    in
      println ("the type t does" ^
	       (if U.same (t, r) then " not" else "") ^
	       " require flattening");
      println ("t:  " ^ toStr t);
      println ("r:  " ^ toStr r)
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

(* let's concoct some datatypes for experiments... *)

  fun newTyc s = TyCon.newDataTyc (Atom.atom s, [])
  fun newCon tyc name t = DataCon.new tyc (Atom.atom name, SOME t)
  fun mkDT (tycName, dconArgs) = let
    val tyc = newTyc tycName
    fun lp (_, []) = tyc
      | lp (i, t::ts) = let
          val con = newCon tyc ("C" ^ Int.toString i) t
          in
	    lp (i+1, ts)
	  end
    in
      lp (1, dconArgs)
    end
   fun tst tyc = mkTest (T.ConTy ([], tyc))
   fun testDT (tycName, dconArgs) = let
     val c = mkDT (tycName, dconArgs)
     in
       mkTest (T.ConTy ([], c))
     end

(* datatype d = C1 of d parray | C2 of d list *)
  val dTyc = newTyc "d"
  val _ = newCon dTyc "C1" (B.parrayTy (T.ConTy ([], dTyc)))
  val _ = newCon dTyc "C2" (B.listTy B.intTy)
  val test93 = tst dTyc

(* datatype h = X1 of int parray | X2 of int *)
  val test94 = testDT ("h", [B.parrayTy B.intTy, B.intTy])

(* datatype s = X1 of int | X2 of s parray *)
  val sTyc = TyCon.newDataTyc (Atom.atom "s", [])
  val dconX1 = DataCon.new sTyc (Atom.atom "X1", SOME (B.intTy))
  val dconX2 = DataCon.new sTyc (Atom.atom "X2", SOME (B.parrayTy (T.ConTy ([], sTyc))))

  val test95 = mkTest (T.ConTy ([], sTyc))

(* datatype r = X1 of int | X2 of int list list | X3 of r | X4 of r * int parray *)
  val rTyc = TyCon.newDataTyc (Atom.atom "r", [])
  val dconX1 = DataCon.new rTyc (Atom.atom "X1", SOME (B.intTy))
  val dconX2 = DataCon.new rTyc (Atom.atom "X2", SOME (B.listTy (B.listTy B.intTy)))
  val dconX3 = DataCon.new rTyc (Atom.atom "X3", SOME (T.ConTy ([], rTyc)))
  val dconX4 = DataCon.new rTyc (Atom.atom "X4", SOME (T.TupleTy [T.ConTy ([], rTyc), 
								  B.parrayTy B.intTy]))

  val test96 = mkTest (T.ConTy ([], rTyc))

(* datatype q = X1 of int | X2 of int list list | X3 of q *)
  val qTyc = TyCon.newDataTyc (Atom.atom "q", [])
  val dconX1 = DataCon.new qTyc (Atom.atom "X1", SOME (B.intTy))
  val dconX2 = DataCon.new qTyc (Atom.atom "X2", SOME (B.listTy (B.listTy B.intTy)))
  val dconX3 = DataCon.new qTyc (Atom.atom "X3", SOME (T.ConTy ([], qTyc)))

  val test97 = mkTest (T.ConTy ([], qTyc))

(* datatype x = X1 of int | X2 of int list list | X3 of int parray *)
  val xTyc = TyCon.newDataTyc (Atom.atom "x", [])
  val dconX1 = DataCon.new xTyc (Atom.atom "X1", SOME (B.intTy))
  val dconX2 = DataCon.new xTyc (Atom.atom "X2", SOME (B.listTy (B.listTy B.intTy)))
  val dconX3 = DataCon.new xTyc (Atom.atom "X3", SOME (B.parrayTy B.intTy))

  val test98 = mkTest (T.ConTy ([], xTyc))

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
