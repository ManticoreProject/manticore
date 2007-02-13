(* label-code-fn.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Lookup information attached to labels.
 *)

functor LabelCodeFn (
	structure MTy : MLRISC_TYPES
) : LABEL_CODE = struct

  structure MTy = MTy
  structure T = MTy.T
  structure P = PropList
  structure LV = CFG.Label

  local
      val {getFn, ...} = 
	  LV.newProp (fn v => 
	    let val atm = CFG.Label.nameOf v
	    in 
		Label.label (Atom.toString atm) ()
	    end)
  in
    fun getName v = getFn v
  end (* local *)

  local
      val {getFn, peekFn, ...} = LV.newProp (fn _ => ref [])
  in
  fun setParamRegs (v, ps : MTy.mlrisc_reg list) = (getFn v) := ps

  fun getParamRegs v =
      (case peekFn v
	of NONE => (*if LV.kindOf v = CFG.LK_Extern "callGC" then []
		  else *) raise Fail ("paramRegs" ^ LV.toString v)
	 | SOME prRef => !prRef
      (* esac *))
  end (* local *)

end (* LabelCodeFn *)
