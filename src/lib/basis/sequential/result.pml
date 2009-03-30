(* result.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure Result =
  struct

    datatype 'a result
      = RES of 'a
      | EXN of exn

  end
