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
 *      fun fib (i, k, exh) = if (i <= 0) then k(0)
 *                       else if (i = 1) then k(1)
 *                       else let
 *			   fun k' a = let
 *			       fun k'' b = k(a+b)
 *			       in
 *				 fib (i-2, k'', exh)
 *			       end
 *			   in
 *			     fib (i-1, k', exh)
 *			   end
 *
 * First-order CPS
 *
 *	fun fib (i, k, cl, exh) = if (i <= 0) then k(0, cl)
 *                       else if (i = 1) then k(1, cl)
 *			 else let
 *			    val cl' = (i, k, cl, exh)
 *			    in
 *			      fib (i-1, k', cl', exh)
 *			    end
 *	and k' (a, cl') = let
 *		val cl''  = (a, #2 cl', #3 cl')
 *		in
 *		  fib ((#1 cl')-2, k'', cl'', #4 cl')
 *		end
 *	and k'' (b, cl'') = (#2 cl'') (#1 cl'' + b, #3 cl'')
 *
 *)

structure Fib =
  struct

    structure P = Prim
    structure Ty = CFGTy

    fun var (name, ty) = CFG.Var.new(Atom.atom name, CFG.VK_None, ty)
    fun newLab ty = CFG.Label.new(Atom.atom "L", CFG.Local, ty)

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
    val bTy = Ty.T_Bool
    val aTy = Ty.T_Any
    val f2aiTy = Ty.T_Code[aTy, iTy]	(* (_ * int) code *)
    val tif2aiaTy = Ty.T_Tuple[iTy, f2aiTy, aTy]
    val tif2aiaaTy = Ty.T_Tuple[iTy, f2aiTy, aTy, aTy]

  (* labels *)
    val fib = CFG.Label.new(Atom.atom "fib", CFG.Export "_fib", aTy)
    val L1 = newLab aTy
    val L2 = newLab aTy
    val L3 = newLab aTy
    val L4 = newLab aTy
    val k' = CFG.Label.new(Atom.atom "k'", CFG.Local, aTy)
    val k'' = CFG.Label.new(Atom.atom "k''", CFG.Local, aTy)

    val code = [
	    func (fib, [("i", iTy), ("k", f2aiTy), ("cl", aTy), ("exh", aTy)], fn [i, k, cl, exh] =>
	      mkLet(iTy, lit 0, fn t =>
	      mkLet(iTy, lte(i, t), fn cond =>
		mkExit(CFG.If(cond, (L1, [k, cl]), (L2, [i, k, cl, exh])))))),
	    xbb (L1, [("k", f2aiTy), ("cl", aTy)], fn [k, cl] =>
	      mkLet(iTy, lit 0, fn t =>
		mkExit(CFG.StdThrow{k=k, arg=t, clos=cl}))),
	    xbb (L2, [("i", iTy), ("k", f2aiTy), ("cl", aTy), ("exh", aTy)], fn [i, k, cl, exh] =>
	      mkLet(iTy, lit 1, fn t =>
	      mkLet(bTy, eq(i, t), fn cond =>
		mkExit(CFG.If(cond, (L3, [k, cl]), (L4, [i, k, cl, exh])))))),
	    xbb (L3, [("k", f2aiTy), ("cl", aTy)], fn [k, cl] =>
	      mkLet(iTy, lit 1, fn t =>
		mkExit(CFG.StdThrow{k=k, arg=t, clos=cl}))),
	    xbb (L4, [("i", iTy), ("k", f2aiTy), ("cl", aTy), ("exh", aTy)], fn [i, k, cl, exh] =>
	      mkLet(iTy, lit 1, fn t =>
	      mkLet(iTy, sub(i, t), fn t' =>
	      mkLet(tif2aiaaTy, alloc[i, k, cl, exh], fn cl' =>
	      mkLet(aTy, label fib, fn fib =>
	      mkLet(aTy, label k', fn k' =>
		 mkExit(CFG.StdApply{f=fib, arg=t', ret=k', clos=cl', exh=exh}))))))),
	    cont(k', [("a", iTy), ("cl'", aTy)], fn [a, cl'] =>
	      mkLet(iTy, select(0, cl'), fn i =>
	      mkLet(iTy, lit 2, fn two =>
	      mkLet(iTy, sub(i, two), fn t =>
	      mkLet(f2aiTy, select(1, cl'), fn k =>
	      mkLet(aTy, select(2, cl'), fn cl =>
	      mkLet(tif2aiaTy, alloc[a, k, cl], fn cl'' =>
	      mkLet(aTy, label fib, fn fib =>
	      mkLet(aTy, label k'', fn k'' =>
	      mkLet(aTy, select(3, cl'), fn exh' =>
		mkExit(CFG.StdApply{f=fib, arg=t, ret=k'', clos=cl'', exh=exh'}))))))))))),
	    cont(k'', [("b", iTy), ("cl''", aTy)], fn [b, cl''] =>
	      mkLet(f2aiTy, select(1, cl''), fn k =>
	      mkLet(aTy, select(0, cl''), fn a =>
	      mkLet(iTy, add(a, b), fn t =>
	      mkLet(aTy, select(2, cl''), fn cl =>
		mkExit(CFG.StdThrow{k=k, arg=t, clos=cl}))))))
	  ]

    val module = CFG.mkModule code

  end
