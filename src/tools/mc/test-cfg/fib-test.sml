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
 *      fun fib (i, k) = if (i <= 0) then k(0)
 *                       else if (i = 1) then k(1)
 *                       else let
 *			   fun k' a = let
 *			       fun k'' b = k(a+b)
 *			       in
 *				 fib (i-2, k'')
 *			       end
 *			   in
 *			     fib (i-1, k')
 *			   end
 *
 * First-order CPS
 *
 *	fun fib (i, k, cl) = if (i <= 0) then k(0, cl)
 *                       else if (i = 1) then k(1, cl)
 *			 else let
 *			    val cl' = (i, k, cl)
 *			    in
 *			      fib (i-1, k', cl')
 *			    end
 *	and k' (a, cl') = let
 *		val cl''  = (a, #2 cl', #3 cl')
 *		in
 *		  fib ((#1 cl')-2, k'', cl'')
 *		end
 *	and k'' (b, cl'') = (#2 cl'') (#1 cl'' + b, #3 cl'')
 *
 *)

structure Fib =
  struct

    structure P = Prim

    fun var name = CFG.Var.new(Atom.atom name, CFG.VK_None, ())
    fun label () = CFG.Label.new(Atom.atom "L", CFG.Local, ())

    fun func (lab, params, body) = let
	  val params = List.map var params
	  in
	    CFG.FUNC{lab=lab, kind=CFG.StandardFunc, params=params, body=body params}
	  end

    fun cont (lab, params, body) = let
	  val params = List.map var params
	  in
	    CFG.FUNC{lab=lab, kind=CFG.ContFunc, params=params, body=body params}
	  end

    fun xbb (lab, params, body) = let
	  val params = List.map var params
	  in
	    CFG.FUNC{lab=lab, kind=CFG.KnownFunc, params=params, body=body params}
	  end

    fun mkLet (rhs, body) = let
	  val tmp = var "_t"
	  in
	    CFG.mkLet([tmp], rhs, body tmp)
	  end

    fun lit i = CFG.E_Literal(Literal.Int i)

  (* prim ops *)
    fun lte (a, b) = CFG.E_Prim(P.I32Lte(a, b))
    fun eq (a, b) = CFG.E_Prim(P.I32Eq(a, b))
    fun add (a, b) = CFG.E_Prim(P.I32Add(a, b))
    fun sub (a, b) = CFG.E_Prim(P.I32Sub(a, b))

  (* labels *)
    val fib = CFG.Label.new(Atom.atom "fib", CFG.Export "_fib", ())
    val L1 = label()
    val L2 = label()
    val L3 = label()
    val L4 = label()
    val k' = CFG.Label.new(Atom.atom "k'", CFG.Local, ())
    val k'' = CFG.Label.new(Atom.atom "k''", CFG.Local, ())

    val code = [
	    func (fib, ["i", "k", "cl"], fn [i, k, cl] =>
	      mkLet(lit 0, fn t =>
	      mkLet(lte(i, t), fn cond =>
		CFG.mkIf(cond, (L1, [k, cl]), (L2, [i, k, cl]))))),
	    xbb (L1, ["k", "cl"], fn [k, cl] => mkLet(lit 0, fn t => CFG.mkThrow(k, [t, cl]))),
	    xbb (L2, ["i", "k", "cl"], fn [i, k, cl] =>
	      mkLet(lit 1, fn t =>
	      mkLet(eq(i, t), fn cond =>
		CFG.mkIf(cond, (L3, [k, cl]), (L4, [i, k, cl]))))),
	    xbb (L3, ["k", "cl"], fn [k, cl] => mkLet(lit 1, fn t => CFG.mkThrow(k, [t, cl]))),
	    xbb (L4, ["i", "k", "cl"], fn [i, k, cl] =>
	      mkLet(lit 1, fn t =>
	      mkLet(sub(i, t), fn t' =>
	      mkLet(CFG.E_Alloc((), [i, k, cl]), fn cl' =>
	      mkLet(CFG.E_Label fib, fn fib =>
	      mkLet(CFG.E_Label k', fn k' =>
		CFG.mkApply(fib, [t', k', cl']))))))),
	    cont(k', ["a", "cl'"], fn [a, cl'] =>
	      mkLet(CFG.E_Select(0, cl'), fn i =>
	      mkLet(lit 2, fn two =>
	      mkLet(sub(i, two), fn t =>
	      mkLet(CFG.E_Select(1, cl'), fn k =>
	      mkLet(CFG.E_Select(2, cl'), fn cl =>
	      mkLet(CFG.E_Alloc((), [a, k, cl]), fn cl'' =>
	      mkLet(CFG.E_Label fib, fn fib =>
	      mkLet(CFG.E_Label k'', fn k'' =>
		CFG.mkApply(fib, [t, k'', cl''])))))))))),
	    cont(k'', ["b", "cl''"], fn [b, cl''] =>
	      mkLet(CFG.E_Select(1, cl''), fn k =>
	      mkLet(CFG.E_Select(0, cl''), fn a =>
	      mkLet(add(a, b), fn t =>
	      mkLet(CFG.E_Select(2, cl''), fn cl =>
		CFG.mkThrow(k, [t, cl]))))))
	  ]

    val module = CFG.mkModule code

  end
