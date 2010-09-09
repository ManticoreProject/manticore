(* manticore-frame-sig.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Handles stack frames for calls and spills.
 *)

signature MANTICORE_FRAME = sig
    
    eqtype loc
    
    val hashLoc : loc -> word
    val locToString : loc -> string
    
    type frame_sz_info

    val newFrameSzInfo : {argSz : int, resSz : int} -> frame_sz_info

    val recordSpill  : (frame_sz_info * int) -> loc
    val recordFSpill  : (frame_sz_info * int) -> loc

    val frameOffset : loc -> int

end
