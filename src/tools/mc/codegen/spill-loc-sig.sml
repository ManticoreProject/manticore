(* spill-loc-sig.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Spill registers to the stack.
 *)

signature SPILL_LOC = sig

    type frame
    structure Frame : MANTICORE_FRAME

    val frameSzInfo : frame -> Frame.frame_sz_info
    val frameAn : frame Annotations.property

end (* SPILL_LOC *)
