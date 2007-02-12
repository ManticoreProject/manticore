(* specialize-calls.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Using control-flow information, specialize calling conventions.
 *)

structure SpecializeCalls : sig

    val transform : CFG.module -> CFG.module

  end = struct

    structure LSet = CFG.Label.Set

  (* equivalence classes of known function labels *)
    type eq_cls = LSet.set URef.uref

    val union = URef.unify LSet.union

  (* tag labels with their equivalence class *)
    val {setFn, peekFn, clrFn, ...} =
	  CFG.Label.newProp (fn lab => URef.uRef(LSet.singleton lab))

    fun analyze code = let
	  fun doFunc (CFG.FUNC{lab, ...}) = let
		fun computeEqCls ) = (case CFA.callSitesOf lab
		       of Known s => let
			    val eqCls = getEqCls lab
			    fun merge lab' = 
			| Unknown _ => ()
		      (* end case *))
		in
		  case entry
		   of CFG.StdFunc _ => computeEqCls()
		    | CFG.KnownFunc _ => computeEqCls()
		    | _ => ()
		  (* end case *)
		end
  end
