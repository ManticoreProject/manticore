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

  val ln = (fn () => print "\n")
  val println = (fn s => (print s; ln ()))

  val intTy = B.intTy
  val unitTy = B.unitTy

  fun mkTest e = (fn () => let
    val (_, f) = FlattenTerms.flatten e
    val g = FlattenOpFusion.fuseExp f
    in
      println ("************ original term:");
      PrintAST.printExp e;
      ln ();
      println ("  : " ^ U.toString (TypeOf.exp e));
      println ("************ flattened term:");
      PrintAST.printExp f;
      ln ();
      println ("  : " ^ U.toString (TypeOf.exp f));
      println ("************ fused term:");
      PrintAST.printExp g;
      ln ();
      println ("  : " ^ U.toString (TypeOf.exp g))
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

  val test5 = let
    val tup = A.TupleExp [ASTUtil.mkInt 0, ASTUtil.mkInt 1]
    val tup' = A.TupleExp [tup, ASTUtil.mkInt 2]
    val ps = A.PArrayExp ([tup'], TypeOf.exp tup')
    in
      mkTest ps
    end

  val test6 = let
    val list = ASTUtil.mkList (List.tabulate (5, ASTUtil.mkInt), intTy)
    in
      fn () => FlattenTypes.mustFlattenTy (FlattenEnv.mkEnv(), TypeOf.exp list)
    end

end
