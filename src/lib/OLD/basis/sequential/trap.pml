(* trap.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Traps are a way to trap exceptions, for later raising, or not.
 * They are used in the translation of pcase.
 *)

structure Trap = struct

  datatype 'a trap 
    = Val of 'a 
    | Exn of exn

end
