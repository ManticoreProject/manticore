(* free-vars.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure FreeVars : sig

    val analyze : CPS.module -> unit

    val freeVarsOf : CPS.var -> CPS.Var.Set.set

  end = struct

    structure V = CPS.Var

    val {getFn = freeVarsOf, setFn, ...} = V.newProp (fn _ => Var.Set.empty)

    fun remove (s, x) = V.Set.delete (s, x) handle _ => s

  (* extend a set of free variables by the variables in a RHS *)
    fun fvOfRHS (fv, CPS.Var xs) = V.Set.addList(fv, xs)
      | fvOfRHS (fv, CPS.Literal _) = fv
      | fvOfRHS (fv, CPS.Select(_, x)) = V.Set.add(fv, x)
      | fvOfRHS (fv, CPS.Alloc xs) = V.Set.addList(fv, xs)
      | fvOfRHS (fv, CPS.Wrap x) = V.Set.add(fv, x)
      | fvOfRHS (fv, CPS.Unwrap x) = V.Set.add(fv, x)
      | fvOfRHS (fv, CPS.Prim p) = V.Set.addList(fv, PrimUtil.varsOf p)
      | fvOfRHS (fv, CPS.CCall(f, args)) = V.Set.addList(fv, f::args)

    fun analExp (fv, e) = (case e
	   of Let([x], rhs, e) => remove(analExp (fvOfRHS (fv, rhs), e), x)
	    | Fun(fbs, e) => ??
	    | Cont(fb, e) => (
		val fvOfLambda = analFB fb
		in
		  setFn (#1 fb, fvOfLambda);
		  remove (analExp (V.Set.union (fv, fvOfLambda), e), #1 fb)
		end
	    | If(x, e1, e2) => analExp (analExp (V.Set.add (fv, x), e1), e2)
	    | Switch _ => raise Fail "switch not implemented"
	    | Apply(f, args) => V.Set.addList(fv, f::args)
	    | Throw(k, args) => V.Set.addList(fv, k::args)
	  (* end case *))

  (* compute the free variables of a lambda; the resulting set may include
   * the lambda's name.
   *)
    and analFB (f, params, body) = V.Set.difference (
	  analExp (V.Set.empty, body),
	  V.Set.addList(V.Set.empty, params))

    fun analyze (CPS.MODULE lambda) =

  end
