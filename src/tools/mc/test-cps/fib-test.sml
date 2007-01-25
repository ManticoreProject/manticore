(* fib-test.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * SML fib function:
 *
 *	fun fib i = if (i <= 0) then 0
 *                  else if (i = 1) then 1
 *                  else fib(i-1) + fib(i-2)
 *
 * CPS converted form
 *
 *      fun fib (i, k, exh) = let
 *	      fun fib' (i, k) = if (i <= 0) then k(0)
 *		    else if (i = 1) then k(1)
 *		      else let
 *			cont k' a = let
 *			      cont k'' b = k(a+b)
 *			      in
 *				fib' (i-2, k'')
 *			      end
 *			in
 *			  fib' (i-1, k')
 *			end
 *            cont k''' i = k (wrap i)
 *            in
 *		fib' (unwrap i, k''')
 *	      end
 *
 *)

structure Fib =
  struct

    structure P = Prim
    structure Ty = CPSTy

    fun var (name, ty) = CPS.Var.new(Atom.atom name, CFG.VK_None, ty)

    fun lit i x = CFG.mkLiteral(x, Literal.Int i)
    fun alloc args x = CFG.mkAlloc(x, args)
    fun label lab x = CFG.mkLabel(x, lab)
    fun select (i, y) x = CFG.mkSelect(x, i, y)

  (* prim ops *)
    fun lte (a, b) x = CFG.mkPrim(x, P.I32Lte(a, b))
    fun eq (a, b) x = CFG.mkPrim(x, P.I32Eq(a, b))
    fun add (a, b) x = CFG.mkPrim(x, P.I32Add(a, b))
    fun sub (a, b) x = CFG.mkPrim(x, P.I32Sub(a, b))

  (* some types *)
    val iTy = Ty.T_Raw Ty.T_Int
    val wiTy = Ty.T_Wrap(Ty.T_Raw Ty.T_Int)
    val bTy = Ty.T_Bool
    val aTy = Ty.T_Any
    val f2aiTy = Ty.T_Code[aTy, iTy]	(* (_ * int) code *)
    val tif2aiaTy = Ty.T_Tuple[iTy, f2aiTy, aTy]
    val tif2aiaaTy = Ty.T_Tuple[iTy, f2aiTy, aTy, aTy]

  (* labels *)
    val fib = CFG.Label.new(Atom.atom "fib", CFG.Export "_fib", aTy)
    val fib' = CFG.Label.new(Atom.atom "fib", CFG.Export "_fib", aTy)
    val k' = CFG.Label.new(Atom.atom "k'", CFG.Local, aTy)
    val k'' = CFG.Label.new(Atom.atom "k''", CFG.Local, aTy)
    val k''' = CFG.Label.new(Atom.atom "k''", CFG.Local, aTy)

    val body = mkLambda(fib, [("i", wiTy), ("k", aTy), ("exh", aTy)], fn (i, k, exh) =>
	  mkFun([
	      mkLambda(fib', [("i", iTy), ("k", aTy)], fn [i, k] =>
		mkLet("z", lit 0, fn z =>
		mkLet("lte0", bTy, lte(i, z), fn lte0 =>
		CPS.If(lte0,
		  mkLet("z", lit 0, fn z => CPS.Throw(k, z)),
		(* else *)
		  mkLet("o", lit 1, fn one =>
		  mkLet("eq1", bTy, eq(i, one), fn eq1 =>
		  CPS.If(eq1,
		    mkLet("o", lit 1, fn one => CPS.Throw(k, one)),
		  (* else *)
		    mkCont(
		      mkLambda(k', [("a", iTy)], fn [a] =>
			mkLet("t", lit 2, fn two =>
			mkLet("im2", sub(i, two), fn im2 =>
			mkCont(
			  mkLambda(k'', [("b", iTy)], fn b =>
			    mkLet("t", iTy, add(a, b), fn t =>
			    CPS.Throw(k, [t]))),
			  CPS.Apply(fib', [im2, k'']))))),
		      fn k' =>
			mkLet("o", lit 1, fn one =>
			mkLet("im1", sub(i, one), fn im1 =>
			CPS.Apply(fib', [im1, k']))))))))))),
	      mkLambda(k''', [("i", iTy)], fn [i] =>
		mkLet("t", wiTy, CPS.E_Wrap i, fn t => CPS.Throw(k, t)))
	    ], fn [fib', k'''] =>
	      mkLet("t", iTy, CPS.E_Unwrap i, fn t => CPS.Apply(fib', [t, k''']))))

    val module = CFG.mkModule body

  end
