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
    structure Ty = CFGTy

    fun var (name, ty) = CFG.Var.new(Atom.atom name, CFG.VK_None, ty)
    fun label ty = CFG.Label.new(Atom.atom "L", CFG.Local, ty)

    fun func (lab, params, bodyFn) = let
	  val params as [clos, arg, ret, exh] = List.map var params
	  val (body, exit) = bodyFn params
	  in
	    CFG.mkFunc (lab, CFG.StdFunc{clos=clos, arg=arg, ret=ret, exh=exh}, body, exit)
	  end

    fun cont (lab, params, bodyFn) = let
	  val params as [clos, arg] = List.map var params
	  val (body, exit) = bodyFn params
	  in
	    CFG.mkFunc (lab, CFG.StdCont{clos=clos, arg=arg}, body, exit)
	  end

    fun known (lab, params, bodyFn) = let
	  val params = List.map var params
	  val (body, exit) = bodyFn params
	  in
	    CFG.mkFunc (lab, CFG.KnownFunc params, body, exit)
	  end

    fun xbb (lab, params, bodyFn) = let
	  val params = List.map var params
	  val (body, exit) = bodyFn params
	  in
	    CFG.mkFunc (lab, CFG.Block params, body, exit)
	  end

    fun mkLet (ty, rhs, bodyFn) = let
	  val tmp = var ("_t", ty)
	  val (code, xfer) = bodyFn tmp
	  in
	    ((rhs tmp) :: code, xfer)
	  end

    fun mkExit xfer = ([], xfer)

    fun lit i x = CFG.E_Literal(x, Literal.Int i)

  (* prim ops *)
    fun lte (a, b) x = CFG.E_Prim(x, P.I32Lte(a, b))
    fun eq (a, b) x = CFG.E_Prim(x, P.I32Eq(a, b))
    fun add (a, b) x = CFG.E_Prim(x, P.I32Add(a, b))
    fun sub (a, b) x = CFG.E_Prim(x, P.I32Sub(a, b))

  (* some types *)
    val iTy = Ty.T_Raw Ty.T_Int
    val aTy = Ty.T_Any
    val faiTy = Ty.T_Fun[aTy, iTy]	(* (_ * int) func *)

  (* labels *)
    val fib = CFG.Label.new(Atom.atom "fib", CFG.Export "_fib", aTy)
    val L1 = label aTy
    val L2 = label aTy
    val L3 = label aTy
    val L4 = label aTy
    val k' = CFG.Label.new(Atom.atom "k'", CFG.Local, aTy)
    val k'' = CFG.Label.new(Atom.atom "k''", CFG.Local, aTy)

    val code = [
	    func (fib, [("i", iTy), ("k", faiTy), ("cl", aTy), ("exh", aTy)], fn [i, k, cl, exh] =>
	      mkLet(iTy, lit 0, fn t =>
	      mkLet(iTy, lte(i, t), fn cond =>
		mkExit (CFG.If(cond, (L1, [k, cl]), (L2, [i, k, cl]))))))(*,
	    xbb (L1, ["k", "cl"], fn [k, cl] => mkLet(lit 0, fn t => CFG.mkThrow(k, [t, cl]))),
	    xbb (L2, ["i", "k", "cl"], fn [i, k, cl] =>
	      mkLet(lit 1, fn t =>
	      mkLet(eq(i, t), fn cond =>
		CFG.mkIf(cond, (L3, [k, cl]), (L4, [i, k, cl]))))),
	    xbb (L3, ["k", "cl"], fn [k, cl] => mkLet(lit 1, fn t => CFG.mkThrow(k, [t, cl]))),
	    xbb (L4, ["i", "k", "cl"], fn [i, k, cl] =>
	      mkLet(lit 1, fn t =>
	      mkLet(sub(i, t), fn t' =>
	      mkLet(CFG.E_Alloc(ty, [i, k, cl]), fn cl' =>
	      mkLet(CFG.E_Label fib, fn fib =>
	      mkLet(CFG.E_Label k', fn k' =>
		CFG.mkApply(fib, [t', k', cl']))))))),
	    cont(k', ["a", "cl'"], fn [a, cl'] =>
	      mkLet(CFG.E_Select(0, cl'), fn i =>
	      mkLet(lit 2, fn two =>
	      mkLet(sub(i, two), fn t =>
	      mkLet(CFG.E_Select(1, cl'), fn k =>
	      mkLet(CFG.E_Select(2, cl'), fn cl =>
	      mkLet(CFG.E_Alloc(ty, [a, k, cl]), fn cl'' =>
	      mkLet(CFG.E_Label fib, fn fib =>
	      mkLet(CFG.E_Label k'', fn k'' =>
		CFG.mkApply(fib, [t, k'', cl''])))))))))),
	    cont(k'', ["b", "cl''"], fn [b, cl''] =>
	      mkLet(CFG.E_Select(1, cl''), fn k =>
	      mkLet(CFG.E_Select(0, cl''), fn a =>
	      mkLet(add(a, b), fn t =>
	      mkLet(CFG.E_Select(2, cl''), fn cl =>
		CFG.mkThrow(k, [t, cl]))))))
*)
	  ]

    val module = CFG.mkModule code

  end
