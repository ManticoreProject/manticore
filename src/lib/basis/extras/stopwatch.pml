(* stopwatch.pml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Stopwatch = struct

  (* time : (unit -> 'a) -> 'a * Time.time *)
  (* Pass in a suspended computation; get back the result and the time it took. *)
  (* Note: time is represented as a long. The unit is microseconds. *)
    fun time thunk = let
      val b = Time.now ()
      val x = thunk ()
      val e = Time.now ()
      in
        (x, e-b)
      end

end
