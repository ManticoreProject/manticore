(* amd64-const.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

functor AMD64ConstantFn (structure AMD64Frame : MANTICORE_FRAME) =
  struct

    datatype const = StackLoc of {
	frame : AMD64Frame.frame_sz_info,
	loc : AMD64Frame.loc
      }
  
    fun toString _ = ""
    fun valueOf (StackLoc {loc, ...}) = AMD64Frame.frameOffset loc
    fun hash _ = 0w0
    fun == _ = true
  
  end (* AMD64Const *)
