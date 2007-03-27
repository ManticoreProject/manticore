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

    structure V = CPS.Var

(* +DEBUG *)
    fun prSet s = (
	  print "{";
	  V.Set.foldl
	    (fn (x, false) => (print("," ^ V.toString x); false)
	      | (x, true) => (print(V.toString x); false)
	    ) true s;
	  print "}")
(* -DEBUG*)

    val {getFn = getFV, setFn = setFV, ...} = V.newProp (fn _ => V.Set.empty)

  (* is a variable externally bound? *)
    fun isExtern x = (case V.kindOf x
	   of CPS.VK_Extern _ => true
	    | _ => false
	  (* end case *))

  (* functions to add free variables to a set; if the variable is extern,
   * then it is ignored.
   *)
    fun addVar (fv, x) = if isExtern x then fv else V.Set.add(fv, x)
    fun addVars (fv, []) = fv
      | addVars (fv, x::xs) = addVars(addVar(fv, x), xs)

    fun remove (s, x) = V.Set.delete (s, x) handle _ => s
    fun removes (s, xs) = List.foldl (fn (x, s) => remove (s, x)) s xs

  (* extend a set of free variables by the variables in a RHS *)
    fun fvOfRHS (fv, CPS.Var xs) = addVars(fv, xs)
      | fvOfRHS (fv, CPS.Enum _) = fv
      | fvOfRHS (fv, CPS.Cast(_, y)) = addVar(fv, y)
      | fvOfRHS (fv, CPS.Literal _) = fv
      | fvOfRHS (fv, CPS.Select(_, x)) = addVar(fv, x)
      | fvOfRHS (fv, CPS.Alloc xs) = addVars(fv, xs)
      | fvOfRHS (fv, CPS.Wrap x) = addVar(fv, x)
      | fvOfRHS (fv, CPS.Unwrap x) = addVar(fv, x)
      | fvOfRHS (fv, CPS.Prim p) = addVars(fv, PrimUtil.varsOf p)
      | fvOfRHS (fv, CPS.CCall(f, args)) = addVars(fv, f::args)
      | fvOfRHS (fv, CPS.Dequeue _) = raise Fail "unexpected Dequeue"
      | fvOfRHS (fv, CPS.Enqueue _) = raise Fail "unexpected Enqueue"
      | fvOfRHS (fv, CPS.HostVProc) = fv
      | fvOfRHS (fv, CPS.VPLoad(_, vp)) = addVar(fv, vp)
      | fvOfRHS (fv, CPS.VPStore(_, vp, x)) = addVars(fv, [vp, x])

    fun analExp (fv, e) = (case e
	   of CPS.Let(xs, rhs, e) => removes(analExp (fvOfRHS (fv, rhs), e), xs)
	    | CPS.Fun(fbs, e) => let
	      (* first, compute the union of the free variables of the lambdas *)
		fun f (fb, fv) = V.Set.union(analFB fb, fv)
		val fbEnv = List.foldl f V.Set.empty fbs
	      (* then remove the function names from the free variable set *)
		fun g ((f, _, _), fv) = remove(fv, f)
		val fbEnv = List.foldl g fbEnv fbs
		in
		(* record the environment for the lambdas *)
		  List.app (fn fb => setFV (#1 fb, fbEnv)) fbs;
		(* also remove the function names from the free variables of e *)
		  List.foldl g (analExp (V.Set.union(fv, fbEnv), e)) fbs
		end
	    | CPS.Cont(fb, e) => let
	      (* compute the free variables of the lambda *)
		val fbEnv = analFB fb
	      (* remove the continuation's name from the set *)
		val fbEnv = remove(fbEnv, #1 fb)
		in
		  setFV (#1 fb, fbEnv);
		  remove (analExp (V.Set.union (fv, fbEnv), e), #1 fb)
		end
	    | CPS.If(x, e1, e2) => analExp (analExp (addVar (fv, x), e1), e2)
	    | CPS.Switch(x, cases, dflt) => 
                List.foldl (fn ((_,e), fv) => analExp (fv, e))
                           (let
                               val fv = addVar (fv, x)
                            in 
                               case dflt of
                                  SOME e => analExp (fv, e)
                                | NONE => fv
                            end)
                           cases
	    | CPS.Apply(f, args) => addVars(fv, f::args)
	    | CPS.Throw(k, args) => addVars(fv, k::args)
	  (* the following cases shouldn't happen after optimization! *)
	    | CPS.Run _ => raise Fail "unexpected Run"
	    | CPS.Forward _ => raise Fail "unexpected Forward"
	  (* end case *))

  (* compute the free variables of a lambda; the resulting set may include
   * the lambda's name.
   *)
    and analFB (f, params, body) = V.Set.difference (
	  analExp (V.Set.empty, body),
	  addVars(V.Set.empty, params))

    fun analyze (CPS.MODULE{name, body, ...}) = let
	  val fv = analFB body
	  in
	    if V.Set.isEmpty fv
	      then ()
	      else (
		print(concat["FV(", Atom.toString name, ") = "]);
		prSet fv; print "\n";
		raise Fail "non-closed module")
	  end

    fun envOfFun f = let
	  val fv = getFV f
	  in
(*DEBUG*)print(concat["FV(", V.toString f, ") = "]); prSet fv; print "\n";
	    fv
	  end

    fun freeVarsOfExp exp = let
	  fun analFB (f, _, _) = getFV f
	  fun analExp (fv, e) = (case e
		 of CPS.Let(xs, rhs, e) => removes(analExp (fvOfRHS (fv, rhs), e), xs)
		  | CPS.Fun(fbs, e) => let
		    (* first add the free variables of the lambdas to fv *)
		      fun f (fb, fv) = V.Set.union(analFB fb, fv)
		      val fv = List.foldl f fv fbs
		      in
		      (* remove the function names from the free variables of e *)
			List.foldl (fn (fb, fv) => remove(fv, #1 fb)) (analExp (fv, e)) fbs
		      end
		  | CPS.Cont(fb, e) =>
		      remove (analExp (V.Set.union (fv, analFB fb), e), #1 fb)
		  | CPS.If(x, e1, e2) => analExp (analExp (addVar (fv, x), e1), e2)
		  | CPS.Switch(x, cases, dflt) => 
                      List.foldl (fn ((_,e), fv) => analExp (fv, e))
                                 (let
                                     val fv = addVar (fv, x)
                                  in
                                     case dflt of
                                        SOME e => analExp (fv, e)
                                      | NONE => fv
                                  end)
                                 cases
		  | CPS.Apply(f, args) => addVars(fv, f::args)
		  | CPS.Throw(k, args) => addVars(fv, k::args)
		  | CPS.Run{vp, act, fiber} => addVars(fv, [vp, act, fiber])
		  | CPS.Forward{vp, sign} => addVars(fv, [vp, sign])
		(* end case *))
	  in
	    analExp (V.Set.empty, exp)
	  end

  end
