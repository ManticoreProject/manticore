(* free-vars.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure FreeVars : sig

    val analyze : CPS.module -> unit

  (* return the free variables of a function or continuation variable *)
    val envOfFun : CPS.var -> CPS.Var.Set.set

  (* return the free variables of an expression.  This function should only be
   * called after analyze has been called.
   *)
    val freeVarsOfExp : CPS.exp -> CPS.Var.Set.set

  end = struct

    structure PPt = ProgPt
    structure V = CPS.Var
    structure VSet = V.Set

(* +DEBUG *)
    fun prSet s = (
	  print "{";
	  VSet.foldl
	    (fn (x, false) => (print("," ^ V.toString x); false)
	      | (x, true) => (print(V.toString x); false)
	    ) true s;
	  print "}")
(* -DEBUG*)

    val {getFn = getFV, setFn = setFV, ...} = V.newProp (fn _ => VSet.empty)
    val {getFn = getFVOfPt, setFn = setFVOfPt, ...} = PPt.newProp (fn _ => VSet.empty)

  (* is a variable externally bound? *)
    fun isExtern x = (case V.kindOf x
	   of CPS.VK_CFun _ => true
	    | _ => false
	  (* end case *))

  (* functions to add free variables to a set; if the variable is extern,
   * then it is ignored.
   *)
    fun addVar (fv, x) = if isExtern x then fv else VSet.add(fv, x)
    fun addVars (fv, []) = fv
      | addVars (fv, x::xs) = addVars(addVar(fv, x), xs)

    fun remove (s, x) = VSet.delete (s, x) handle _ => s
    fun removes (s, xs) = List.foldl (fn (x, s) => remove (s, x)) s xs

  (* extend a set of free variables by the variables in a RHS *)
    fun fvOfRHS (fv, CPS.Var xs) = addVars(fv, xs)
      | fvOfRHS (fv, CPS.Const _) = fv
      | fvOfRHS (fv, CPS.Cast(_, y)) = addVar(fv, y)
      | fvOfRHS (fv, CPS.Select(_, x)) = addVar(fv, x)
      | fvOfRHS (fv, CPS.Update(_, x, y)) = addVars(fv, [x, y])
      | fvOfRHS (fv, CPS.AddrOf(_, x)) = addVar(fv, x)
      | fvOfRHS (fv, CPS.Alloc(_, xs)) = addVars(fv, xs)
      | fvOfRHS (fv, CPS.Promote x) = addVar(fv, x)
      | fvOfRHS (fv, CPS.Prim p) = addVars(fv, PrimUtil.varsOf p)
      | fvOfRHS (fv, CPS.CCall(f, args)) = addVars(fv, f::args)
      | fvOfRHS (fv, CPS.HostVProc) = fv
      | fvOfRHS (fv, CPS.VPLoad(_, vp)) = addVar(fv, vp)
      | fvOfRHS (fv, CPS.VPStore(_, vp, x)) = addVars(fv, [vp, x])
      | fvOfRHS (fv, CPS.VPAddr(_, vp)) = addVar(fv, vp)

  (* return the variable of a lambda *)
    fun funVar (CPS.FB{f, ...}) = f

    fun analExp (CPS.Exp(_, e)) = (case e
	   of CPS.Let(xs, rhs, e) => removes (fvOfRHS(analExp e, rhs), xs)
	    | CPS.Fun(fbs, e) => let
	      (* first, compute the union of the free variables of the lambdas *)
		fun f (fb, fv) = VSet.union(analFB fb, fv)
		val fbEnv = List.foldl f VSet.empty fbs
	      (* then remove the function names from the free variable set *)
		fun g (fb, fv) = remove(fv, funVar fb)
		val fbEnv = List.foldl g fbEnv fbs
		in
		(* record the environment for the lambdas *)
		  List.app (fn fb => setFV (funVar fb, fbEnv)) fbs;
		(* also remove the function names from the free variables of e *)
		  List.foldl g (VSet.union(analExp e, fbEnv)) fbs
		end
	    | CPS.Cont(fb, e) => let
	      (* compute the free variables of the lambda *)
		val fbEnv = analFB fb
	      (* remove the continuation's name from the set *)
		val fbEnv = remove(fbEnv, funVar fb)
		in
		  setFV (funVar fb, fbEnv);
		  remove (VSet.union (fbEnv, analExp e), funVar fb)
		end
	    | CPS.If(cond, e1, e2) => let
		val fv1 = analExpAndRecord e1
		val fv2 = analExpAndRecord e2
		in
		  addVars (VSet.union(fv1, fv2), CondUtil.varsOf cond)
		end
	    | CPS.Switch(x, cases, dflt) => let
		fun doCase ((_, e), fv) = VSet.union (fv, analExpAndRecord e)
		val fv = List.foldl doCase VSet.empty cases
		in
		  case dflt
		   of SOME e => VSet.union(fv, analExpAndRecord e)
		    | NONE => fv
		  (* end case *)
		end
	    | CPS.Apply(f, args, rets) => addVars(VSet.empty, f::args@rets)
	    | CPS.Throw(k, args) => addVars(VSet.empty, k::args)
	  (* end case *))

  (* analyze and record the free variables of an expression *)
    and analExpAndRecord (e as CPS.Exp(ppt, _)) = let
	  val fv = analExp e
	  in
	    setFVOfPt (ppt, fv);
	    fv
	  end

  (* compute the free variables of a lambda; the resulting set may include
   * the lambda's name.
   *)
    and analFB (CPS.FB{f, params, rets, body}) = VSet.difference (
	  analExp body,
	  addVars (addVars(VSet.empty, params), rets))

    fun analyze (CPS.MODULE{name, externs, body, ...}) = let
	  val fv = analFB body
	  in
	    if VSet.isEmpty fv
	      then ()
	      else (
		print(concat["FV(", Atom.toString name, ") = "]);
		prSet fv; print "\n";
		raise Fail "non-closed module")
	  end

    val analyze = BasicControl.mkTracePassSimple {
	    passName = "free-vars",
	    pass = analyze
	  }

    fun envOfFun f = let
	  val fv = getFV f
	  in
            if Controls.get ClosureControls.debug
               then (print(concat["FV(", V.toString f, ") = "]); prSet fv; print "\n")
            else ();
	    fv
	  end

    fun freeVarsOfExp (CPS.Exp(ppt, _)) = getFVOfPt ppt

  end
