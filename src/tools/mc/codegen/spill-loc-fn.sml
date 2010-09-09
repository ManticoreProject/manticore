(* spill-loc-fn.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Spill registers to the stack.
 *)

functor SpillLocFn (
	structure Frame : MANTICORE_FRAME
) = struct

  structure Frame = Frame

  datatype frame = FR of {
	   fsi : Frame.frame_sz_info
  }

  local 
      val {getFn, ...} = CFG.Label.newProp (fn v => ref (FR {fsi=
			Frame.newFrameSzInfo {argSz=0, resSz=0}}))
  in
    fun getFuncFrame l = !(getFn l)
  end (* local *)

  fun toString _ = ""
  fun frameSzInfo (FR {fsi, ...}) = fsi
  val frameAn : frame Annotations.property = Annotations.new (SOME toString)

end (* SpillLocFn *)
