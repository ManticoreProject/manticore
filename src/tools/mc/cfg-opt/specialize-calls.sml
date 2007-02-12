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

    structure CFA = CFACFG
    structure LSet = CFG.Label.Set

  (* equivalence classes of known function labels *)
    type eq_cls = LSet.set URef.uref

    val union = URef.unify LSet.union

  (* tag labels with their equivalence class *)
    val {getFn = getEqCls, peekFn, clrFn, ...} =
	  CFG.Label.newProp (fn lab => URef.uRef(LSet.singleton lab))

    fun analyze code = let
	  fun doFunc (CFG.FUNC{lab, entry, ...}) = let
		fun computeEqCls () = (case CFA.callSitesOf lab
		       of CFA.Known s => let
			    val eqCls = getEqCls lab
			  (* merge the call equivalence classes of the other functions
			   * called from the same sites.
			   *)
			    fun merge siteLab = let
				  val CFG.LK_Local{func=CFG.FUNC{exit, ...}, ...} =
					CFG.Label.kindOf siteLab
				  fun mergeEqClasses dst = (case CFA.valueOf dst
					 of CFA.LABELS labs => let
					    (* for each label, merge its equiv class with eqCls *)
					      fun u l = if CFG.Label.same(l, lab)
						      then ()
						      else ignore (union(getEqCls l, eqCls))
					      in
						LSet.app u labs
					      end
					  | _ => raise Fail "not labels"
					(* end case *))
				  in
				    case exit
				     of CFG.StdApply{f, ...} => mergeEqClasses f
				      | CFG.StdThrow{k, ...} => mergeEqClasses k
				      | _ => raise Fail "not standard function/continuation"
				    (* end case *)
				  end
			    in
			      LSet.app merge s
			    end
			| CFA.Unknown => ()
		      (* end case *))
		in
		  case entry
		   of CFG.StdFunc _ => computeEqCls()
		    | CFG.KnownFunc _ => computeEqCls()
		    | _ => ()
		  (* end case *)
		end
	  in
	    List.map doFunc code
	  end (* analyze *)

    fun transform m = m

  end
