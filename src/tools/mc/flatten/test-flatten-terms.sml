(* test-flatten-terms.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure TestFlattenTerms = struct

  structure A = AST
  structure T = Types
  structure B = Basis
  structure U = TypeUtil
  
  val env = FlattenEnv.mkEnv ()

  val println = (fn s => (print s; print "\n"))

  val intTy = B.intTy
  val unitTy = B.unitTy

  fun mkTest e = (fn () => let
    val f = FlattenTerms.exp env e
    val g = Fusion.fuse f
    in
      println ("************ original term:");
      PrintAST.printExp e;
      println ("************ flattened term:");
      PrintAST.printExp f;
      println ("************ fused term:");
      PrintAST.printExp g;
      println ""
    end)

  val test0 = mkTest (A.TupleExp [])

  val test1 = mkTest (A.PArrayExp ([ASTUtil.mkInt 7], intTy))

  val test2 = let
    val ns = List.tabulate (5, ASTUtil.mkInt)
    in
      mkTest (A.PArrayExp (ns, intTy))
    end

  val test3 = let
    val ns1 = A.PArrayExp ([ASTUtil.mkInt 0, ASTUtil.mkInt 1], intTy)
    val ns2 = A.PArrayExp ([ASTUtil.mkInt 2], intTy)
    val nss = A.PArrayExp ([ns1, ns2], B.parrayTy intTy)
    in
      mkTest nss
    end

  val test4 = let
    val tup = A.TupleExp [ASTUtil.mkInt 0, ASTUtil.mkInt 1]
    val ps = A.PArrayExp ([tup], T.TupleTy [intTy, intTy])
    in
      mkTest ps
    end

end
