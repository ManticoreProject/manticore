(* expand-ops-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor ExpandOpsFn (Spec : TARGET_SPEC) : sig

    val transform : CPS.module -> CPS.module

  end = struct

    structure Ty = CPSTy
    structure Offsets = Spec.ABI

    fun var (name, ty) = CPS.Var.new(Atom.atom name, ty)

    fun cfun (name, resTy, paramTys) = let
	  val cf = var(name, Ty.T_CFun(CFunctions.CProto(resTy, paramTys)))
	  in
	    CPS.Var.setKind(cf, CPS.VK_Extern name);
	    CFunctions.CFun{var = cf, name = name, retTy=resTy, argTys = paramTys}
	  end

    fun mkLet ([], e) = e
      | mkLet ((lhs, rhs)::binds, e) = CPS.mkLet(lhs, rhs, mkLet(binds, e))

    fun mkCont (name, param, body, k) = let
	  val name = var(name, Ty.T_Cont[#2 param])
	  val param = var param
	  in
	    CPS.mkCont((name, [param], body param), k name)
	  end

  (* the type of a fiber *)
    val fiberTy = Ty.T_Cont[Ty.unitTy]

  (* the type of a thread ID *)
    val tidTy = Ty.T_Any

  (* the type of items on a vproc's action stack *)
    val actStkItemTy = Ty.T_Tuple[Ty.T_Cont[Ty.T_Any], Ty.T_Any]

  (* the type of items in a vproc's ready queue *)
    val rdyQItemTy = Ty.T_Tuple[tidTy, fiberTy, Ty.T_Any]

  (* expansion for Run *)
    fun xRun {vp, act, fiber} = let
	  val stk = var("stk", actStkItemTy)
	  val item = var("item", actStkItemTy)
	  val f = var("false", Ty.boolTy)
	  val u = var("unit", Ty.unitTy)
	  in
	    mkLet([
		([stk], CPS.VPLoad(Offsets.actionStk, vp)),
		([item], CPS.Alloc[act, stk]),
		([], CPS.VPStore(Offsets.actionStk, vp, item)),
		([f], CPS.falseLit),
		([], CPS.VPStore(Offsets.atomic, vp, f)),
		([u], CPS.unitLit)
	      ],
	      CPS.Throw(fiber, [u]))
	  end

  (* expansion for Forward *)
    fun xForward {vp, sign} = let
	  val tos = var("tos", actStkItemTy)
	  val rest = var("rest", Ty.T_Any)
	  val act = var("act", Ty.T_Cont[Ty.T_Any])
	  in
	    mkLet([
		([tos], CPS.VPLoad(Offsets.actionStk, vp)),
		([rest], CPS.Select(1, tos)),
		([], CPS.VPStore(Offsets.actionStk, vp, rest)),
		([act], CPS.Select(0, tos))
	      ],
	      CPS.Throw(act, [sign]))
	  end

  (* expansion for Enqueue *)
    fun xEnqueue ((vp, tid, fiber), e) = let
	  val tl = var("tl", rdyQItemTy)
	  val item = var("item", rdyQItemTy)
	  in
	    mkLet([
		([tl], CPS.VPLoad(Offsets.rdyQTl, vp)),
		([item], CPS.Alloc[tid, fiber, tl]),
		([], CPS.VPStore(Offsets.rdyQTl, vp, item))
	      ],
	      e)
	  end

  (* expansion for Dequeue *)
    fun xDequeue VProcDequeue (x, vp, e, exh) = let
	  val join = var("join", Ty.T_Cont[CPS.Var.typeOf x])
	  val hd = var("hd", rdyQItemTy)
	  val link = var("link", Ty.T_Any)
	  val isBoxed1 = var("isBoxed", Ty.boolTy)
	  val tl = var("tl", rdyQItemTy)
	  val nil' = var("nil", Ty.T_Enum(0w0))
	  val isBoxed2 = var("isBoxed", Ty.boolTy)
(* FIXME: if CPS supports specialized calling conventions, then we could drop the exh parameter and
 * avoid the tupling of the argument.
 *)
	  fun copyLoop (tl, fastPath)= let
		val param = var("param", Ty.T_Tuple[Ty.T_Any, Ty.T_Any])
		val retK = var("retK", Ty.T_Cont[Ty.T_Any]);
		val loop = var("loop", Ty.T_Fun[CPS.Var.typeOf param, CPS.Var.typeOf retK, CPS.Var.typeOf exh])
		val lst = var("lst", rdyQItemTy)
		val hd = var("hd", Ty.T_Any)
		val lst' = var("lst", Ty.T_Any)
		val tid = var("tid", tidTy)
		val fiber = var("fiber", fiberTy)
		val hd' = var("hd", rdyQItemTy)
		val nil' = var("nil", Ty.T_Enum(0w0))
		val arg = var("arg", CPS.Var.typeOf param)
		val arg' = var("arg", CPS.Var.typeOf param)
		val isBoxed = var("isBoxed", Ty.boolTy)
		in
		  CPS.mkFun([(loop, [param, retK, exh],
		    mkLet([
			([lst], CPS.Select(0, param)),
			([hd], CPS.Select(1, param)),
			([tid], CPS.Select(0, lst)),
			([fiber], CPS.Select(1, lst)),
			([hd'], CPS.Alloc[tid, fiber, hd]),
			([lst'], CPS.Select(2, lst)),
			([isBoxed], CPS.Prim(Prim.isBoxed lst'))
		      ],
		      CPS.If(isBoxed,
			CPS.mkLet([arg'], CPS.Alloc[lst', hd'], CPS.Apply(loop, [arg', retK, exh])),
			CPS.Throw(retK, [hd']))))],
		  mkLet([
		      ([nil'], CPS.nilLit),
		      ([arg], CPS.Alloc[tl, nil'])
		    ],
		    CPS.Apply(loop, [arg, fastPath, exh])))
		end
	  val item = var("item", rdyQItemTy)
	  in
	    CPS.mkCont((join, [x], e),
	    mkCont("fastPath", ("hd", rdyQItemTy),
	      fn hd => mkLet([
		  ([link], CPS.Select(2, hd)),
		  ([], CPS.VPStore(Offsets.rdyQHd, vp, link))
		],
		CPS.Throw(join, [hd])),
	      fn fastPath => mkLet([
		  ([hd], CPS.VPLoad(Offsets.rdyQHd, vp)),
		  ([isBoxed1], CPS.Prim(Prim.isBoxed hd))
		],
		CPS.If(isBoxed1,
		  CPS.Throw(fastPath, [hd]),
		  mkLet([
		      ([tl], CPS.VPLoad(Offsets.rdyQTl, vp)),
		      ([nil'], CPS.nilLit),
		      ([], CPS.VPStore(Offsets.rdyQTl, vp, nil')),
		      ([isBoxed2], CPS.Prim(Prim.isBoxed tl))
		    ],
		    CPS.If(isBoxed2,
		      copyLoop(tl, fastPath),
		      CPS.mkLet([item], CPS.CCall(VProcDequeue, [vp]),
			CPS.Throw(join, [item]))))))))
	  end

    fun transform (CPS.MODULE{name, externs, body}) = let
	  val VProcDequeue = cfun("VProcDequeue", CFunctions.PointerTy, [CFunctions.PointerTy])
	  val xDequeue = xDequeue (CFunctions.varOf VProcDequeue)
	  fun xExp (e, exh) = let
		fun expand (CPS.Let([x], CPS.Dequeue vp, e)) = xDequeue (x, vp, expand e, exh)
		  | expand (CPS.Let([], CPS.Enqueue arg, e)) = xEnqueue (arg, expand e)
		  | expand (CPS.Let(lhs, rhs, e)) = CPS.mkLet(lhs, rhs, expand e)
		  | expand (CPS.Fun(fbs, e)) = CPS.mkFun(List.map xLambda fbs, expand e)
		  | expand (CPS.Cont(fb, e)) = CPS.mkCont(xCont(fb, exh), expand e)
		  | expand (CPS.If(x, e1, e2)) = CPS.If(x, expand e1, expand e2)
		  | expand (CPS.Switch(x, cases, dflt)) =
		      CPS.Switch(x,
			List.map (fn (l, e) => (l, expand e)) cases,
			Option.map expand dflt)
		  | expand (e as CPS.Apply _) = e
		  | expand (e as CPS.Throw _) = e
		  | expand (CPS.Run arg) = xRun arg
		  | expand (CPS.Forward arg) = xForward arg
		in
		  expand e
		end
	  and xLambda (f, params, e) = (f, params, xExp(e, List.last params))
	  and xCont ((k, params, e), exh) = (k, params, xExp(e, exh))
	  in
	    CPS.MODULE{
		name = name, externs = VProcDequeue::externs,
		body = xLambda body
	      }
	  end

  end
