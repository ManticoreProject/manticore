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

  fun getName cl = Label.label (CFG.Label.toString cl) ()

  local
      val {getFn, peekFn, ...} = LV.newProp (fn _ => ref [])
  in

  fun setParamRegs (v, ps : MTy.mlrisc_reg list) = (getFn v) := ps

  fun getParamRegs v =
      (case peekFn v
	of NONE => raise Fail "paramRegs"
	 | SOME prRef => !prRef
      (* esac *))

  end (* local *)

end (* LabelCodeFn *)
