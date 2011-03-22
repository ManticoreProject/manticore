(* test-realize-farray.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure TestRealizeFArray = struct

  structure A = AST
  structure T = Types
  structure B = Basis

  structure AU = ASTUtil
  structure TU = TypeUtil
  
  val intTy = B.intTy
  val unitTy = B.unitTy

  fun ln () = (TextIO.print "\n")
  fun println s = (TextIO.print s; ln ())

  fun mkTest e = (fn () => let
    val e' = RealizeFArray.realize e
    in
      println ("************ original term:");
      PrintAST.printExp e;
      ln ();
      println ("  : " ^ TU.toString (TypeOf.exp e));
      println ("************ transformed term:");
      PrintAST.printExp e';
      ln ();
      println ("  : " ^ TU.toString (TypeOf.exp e'))
    end)

  fun int n = AU.mkInt n
  fun ints ns = map int ns

  val test0 = mkTest (A.TupleExp [])

end
