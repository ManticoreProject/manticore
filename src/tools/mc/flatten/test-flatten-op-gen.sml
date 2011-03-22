(* test-flatten-op-gen.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Tests of FlattenOpGen.
 *
 * Supporting documents in 
 * /path/to/manti-papers/papers/notes/amsft
 *)

structure TestFlattenOpGen = struct

  structure A = AST
  structure B = Basis
  structure T = Types
  structure F = FlattenOp
  structure G = FlattenOpGen
  structure U = TypeUtil

  structure S = FlattenOp.Set

  fun println s = (TextIO.print s; TextIO.print "\n")

  fun mkTest (s : S.set) = (fn () => let
    val (opers, lams) = ListPair.unzip (G.gen s)
    val bogusLet = A.LetExp (A.FunBind lams, A.TupleExp [])
    in
      println "******** input set:";
      FlattenOp.Set.app (fn oper => println (FlattenOp.toString oper)) s;
      println "******** function defs:";
      PrintAST.printExp bogusLet
    end)

  fun fromList opers = List.foldl S.add' S.empty opers

  val test0 = let
    val s = S.singleton (A.ID B.intTy)
    in
      mkTest s
    end

  val test1 = let
    val s = fromList [A.ID B.intTy, A.ID B.boolTy]
    in
      mkTest s
    end

  val test2 = let
    val s = fromList [A.ID B.intTy,
		      A.ID B.boolTy,
		      A.CrossCompose [A.ID B.intTy, A.ID B.boolTy]]
    in
      mkTest s
    end

  val test3 = let
    val ii = A.ID B.intTy
    val s = fromList [ii,
		      A.Compose (ii, ii),
		      A.CrossCompose [ii, ii]]
    in
      mkTest s
    end

  val test4 = let
    val s = S.singleton (A.Unzip (T.TupleTy [B.intTy, B.boolTy]))
    in
      mkTest s
    end

  val test5 = let
    val s = S.singleton (A.Unzip (T.TupleTy [B.intTy, B.boolTy, B.stringTy]))
    in
      mkTest s
    end

  val test6 = let
    val s = S.singleton (A.Map (A.ID B.intTy, T.LfTy))
    in
      mkTest s
    end

  val test7 = let
    val o1 = A.Unzip (T.TupleTy [T.TupleTy [B.intTy, B.boolTy], B.floatTy])
    val o2 = A.Unzip (T.TupleTy [B.intTy, B.boolTy])
    val o3 = A.CrossCompose [A.Unzip (T.TupleTy [B.intTy, B.boolTy]),
			     A.ID B.floatTy]
    val s = fromList [o1, o2, o3, A.ID B.floatTy]
    in
      mkTest s
    end

  val test8 = let
    val {fArrTyc, ...} = FlattenOpGen.spoofBasisItems ()
    val o1 = A.Cat (T.ConTy ([T.ConTy ([B.intTy], fArrTyc)], fArrTyc))
    val s = fromList [o1]
    in
      mkTest s
    end

end
