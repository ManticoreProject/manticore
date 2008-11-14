(* arch.sig
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * N.B. Not actually used in compiler.
 *)

signature ARCH = sig

  val cacheLineSizeBytes : int
  val wordSizeBytes : int

end

