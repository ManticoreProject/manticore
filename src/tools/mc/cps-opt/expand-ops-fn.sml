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

    fun mkLet ([], e) = e
      | mkLet ((lhs, rhs)::binds, e) = CPS.mkLet(lhs, rhs, mkLet(binds, e))

  (* the type of items on a vproc's action stack *)
    val actStkItemTy = Ty.T_Tuple[Ty.T_Cont[Ty.T_Any], Ty.T_Any]

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

    fun transform (CPS.MODULE{name, externs, body}) = let
	  fun xExp (CPS.Let(lhs, rhs, e)) = CPS.mkLet(lhs, rhs, xExp e)
	    | xExp (CPS.Fun(fbs, e)) = CPS.mkFun(List.map xLambda fbs, xExp e)
	    | xExp (CPS.Cont(fb, e)) = CPS.mkCont(xLambda fb, xExp e)
	    | xExp (CPS.If(x, e1, e2)) = CPS.If(x, xExp e1, xExp e2)
	    | xExp (CPS.Switch(x, cases, dflt)) =
		CPS.Switch(x,
		  List.map (fn (l, e) => (l, xExp e)) cases,
		  Option.map xExp dflt)
	    | xExp (e as CPS.Apply _) = e
	    | xExp (e as CPS.Throw _) = e
	  (* scheduler operations *)
	    | xExp (CPS.Run arg) = xRun arg
	    | xExp (CPS.Forward arg) = xForward arg
	  and xLambda (f, params, e) = (f, params, xExp e)
	  in
	    CPS.MODULE{
		name = name, externs = externs,
		body = xLambda body
	      }
	  end

  end
