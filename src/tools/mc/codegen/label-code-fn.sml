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
      fun convert s =
         String.map (fn #"-" => #"_" | c => c) s
      val {getFn, ...} = 
	  LV.newProp (fn v => (case CFG.Label.kindOf v
	      of CFG.LK_Extern s => Label.global s
	       | _ => Label.label(convert (CFG.Label.nameOf v)) ()
            (* esac *)))
  in
    fun getName v = getFn v
  end (* local *)

  local
(* QUESTION: why not use setFn and skip the ref? *)
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
