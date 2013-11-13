(* spec-par.pml
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * support for speculative parallelism that provides runtime support for 
 * rolling back ivars in the event an excpetion is raised
 *)



structure MVar (*: sig
    val spec : (() -> 'a * () -> 'b) -> ('a, 'b)
  end*) = struct

        _primcode(
                @define primSpec
        )

        val spec : (() -> 'a * () -> 'b) -> ('a * 'b) = primSpec




