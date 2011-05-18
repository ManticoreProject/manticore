(* progress.pml  
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Polymorphic progress datatype.
 *)

structure Progress = struct

  datatype ('a, 'b) progress
    = More of 'a
    | Done of 'b

end
