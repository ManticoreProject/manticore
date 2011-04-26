(* zero-pi-bug.pml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This program demonstrates a bug in the type checker.
 *)

structure MyBasis : sig
    val pi : double
end = struct
    val pi =  3.14159265358979323846
end

val _ = Print.printLn (Double.toString MyBasis.pi)
