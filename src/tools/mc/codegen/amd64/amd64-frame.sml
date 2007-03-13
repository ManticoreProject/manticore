(* amd64-frame.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure AMD64Frame = ManticoreFrameFn (
                           val wordSz = 8
			   val floatSz = 12
			   val floatAlign = 4
			   val linkageSz = 6)
