(* test-translate-terms.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure TestTranslateTerms = struct

  structure A = AST
  structure F = FLAST
  structure FEnv = FlattenEnv

  type env = FEnv.env

  val println = (fn s => (print s; print "\n"))

  val env0 = let
    val e = FEnv.mkEnv ()
  (* initialize the env by adding all f-basis elements to it *)
    val _ = FEnv.insertTyc (e, Basis.threadIdTyc, FBasis.threadIdTyc)
    val _ = FEnv.insertTyc (e, Basis.parrayTyc, FBasis.parrayTyc)
    val _ = FEnv.insertTyc (e, Basis.listTyc, FBasis.listTyc)
    val _ = FEnv.insertTyc (e, Basis.intTyc, FBasis.intTyc)
    val _ = FEnv.insertCon (e, Basis.listNil, FBasis.listNil)
    val _ = FEnv.insertCon (e, Basis.listCons, FBasis.listCons)
    in
      e
    end

  fun mkt (env:env) (e:A.exp) : unit->unit = (fn () => let
    val e' = FlattenTranslateTerms.trExp (env0, e)
    in
      println "e before translation:";
      PrintAST.printExp e;
      println "e after transation:";
      PrintFLAST.printExp e'
    end)

  fun mkTest (e : A.exp) : unit -> unit = mkt env0 e

  fun mkTestEnv (env : env, e : A.exp) : unit -> unit = mkt env e

(* some int const expressions for use below *)
  local
    val mk = ASTUtil.mkInt
  in
    val [a0, a1, a2, a3, a4, a5] = List.tabulate (6, mk)
  end

(* int const *)
  val test0 = mkTest a0

(* bool const *)
  val test1 = mkTest (ASTUtil.trueExp)

(* LetExp *)
  val test2 = let
    val x = Var.new ("x", Basis.intTy)
    val b = A.ValBind (A.VarPat x, a1)
    val e = A.LetExp (b, a0)
    in
      mkTest e
    end

(* LetExp, two vars *)
  val test3 = let
    fun intVar name = Var.new (name, Basis.intTy)
    val (x, y) = (intVar "x", intVar "y")
    val bx = A.ValBind (A.VarPat x, a1)
    val by = A.ValBind (A.VarPat y, a2)
    val e = A.LetExp (bx, A.LetExp (by, a3))
    in
      mkTest e
    end

(* empty list *)
  val test4 = let
    val n = ASTUtil.mkList ([], Basis.intTy)
    in
      mkTest n
    end

(* singleton list *)
  val test5 = let
    val s = ASTUtil.mkList ([a0], Basis.intTy)
    in
      mkTest s
    end

(* two-element list *)
  val test6 = let
    val ns = ASTUtil.mkList ([a0,a1], Basis.intTy)
    in
      mkTest ns
    end

(* if *)
  val test7 = let
    val e = A.IfExp (ASTUtil.trueExp, a0, a1, Basis.intTy)
    in
      mkTest e
    end

(* case with one (wildcard) arm *)
  val test8 = let
    val m = A.PatMatch (A.WildPat Basis.intTy, a1)
    val e = A.CaseExp (a0, [m], Basis.intTy)
    in
      mkTest e
    end	
		 
(* case with several arms *)
  val test9 = let
    val m0 = A.PatMatch (ASTUtil.mkIntPat 0, a0)
    val m1 = A.PatMatch (ASTUtil.mkIntPat 1, a2)
    val m2 = A.PatMatch (A.WildPat Basis.intTy, a1)
    val e = A.CaseExp (a0, [m0,m1,m2], Basis.intTy)
    in
      mkTest e
    end

(* pcase *)
  val test10 = let
    val m = A.PMatch ([A.NDWildPat Basis.intTy, A.NDWildPat Basis.intTy], a0)
    val e = A.PCaseExp ([a0, a1], [m], Basis.intTy)
    in
      mkTest e
    end    

(* handle *)
  val test11 = let
    val m = A.PatMatch (A.WildPat Basis.exnTy, a0)
    val e = A.HandleExp (a0, [m], Basis.intTy)
    in
      mkTest e
    end

(* raise *)
  val test12 = let
    val e = A.RaiseExp (ASTUtil.exnMatchExp, Basis.intTy)
    in
      mkTest e
    end

(* anonymous function *)
  val test13 = let
    val x = Var.new ("x", Basis.intTy)
    val e = A.FunExp (x, a0, Basis.intTy)
    in
      mkTest e
    end

(* application *)
  val test14 = let
    val x = Var.new ("x", Basis.intTy)
    val f = A.FunExp (x, A.VarExp (x, []), Basis.intTy)
    val e = A.ApplyExp (f, a2, Basis.intTy)
    in
      mkTest e
    end

(* tuple *)
  val test15 = let
    val e = A.TupleExp [a0, a1, a2]
    in
      mkTest e
    end

(* range *)
  val test16 = let
    val e = A.RangeExp (a0, a5, SOME a1, Basis.intTy)
    in
      mkTest e
    end

(* ptuple *)
  val test17 = let
    val e = A.PTupleExp [a0, a1, a2]
    in
      mkTest e
    end

(* parray *)
  val test18 = let
    val e = A.PArrayExp ([a0, a1, a2], Basis.intTy)
    in
      mkTest e
    end

(* pcomp *)
  val test19 = let
(* Note: if the expression doesn't bind a given variable, then the
 * translation will fail when it hits it -- hence the LetExp.
 *)
    val x = Var.new ("x", Basis.intTy)
    val xs = Var.new ("xs", Basis.parrayTy Basis.intTy)
    val e = A.LetExp (A.ValBind (A.VarPat xs, 
				 A.PArrayExp ([], Basis.intTy)),
		      A.PCompExp (a0, [(A.VarPat x, A.VarExp (xs, []))], 
				  SOME ASTUtil.trueExp))
    in
      mkTest e
    end

(* pchoice *)
  val test20 = let
    val e = A.PChoiceExp ([a0, a1], Basis.intTy)
    in
      mkTest e
    end

(* spawnExp *)
  val test21 = let
    val u = Var.new ("u", Basis.unitTy)
    val f = Var.new ("f", Types.FunTy (Basis.unitTy, Basis.unitTy))
    val func = A.FunExp (u, A.VarExp (u, []), Basis.unitTy)
    val e = A.LetExp (A.ValBind (A.VarPat f, func),
		      A.SpawnExp (A.VarExp (f, [])))
    in
      mkTest e
    end

(* functions *)
  val test22 = let
    val unit = Basis.unitTy
    val f = Var.new ("f", Types.FunTy (unit, unit))
    val g = Var.new ("g", Types.FunTy (unit, unit))
    val u1 = Var.new ("u", unit)
    val u2 = Var.new ("u", unit)
    val lamF = A.FB (f, 
		     u1,
		     A.IfExp (ASTUtil.trueExp, 
			      ASTUtil.unitExp,
			      ASTUtil.mkApplyExp (A.VarExp (g, []),
						  [A.VarExp (u1, [])]),
			      unit))
    val lamG = A.FB (g, 
		     u2,
		     A.IfExp (ASTUtil.trueExp,
			      ASTUtil.unitExp,
			      ASTUtil.mkApplyExp (A.VarExp (f, []),
						  [A.VarExp (u2, [])]),
			      unit))
    val bind = A.FunBind [lamF, lamG]
    val e = A.LetExp (bind, ASTUtil.mkApplyExp (A.VarExp (f, []), 
						[ASTUtil.unitExp]))
    in
      mkTest e
    end

end
