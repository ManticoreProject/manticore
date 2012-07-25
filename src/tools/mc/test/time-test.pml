(* time-test.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Test the time to string functions.
 *)

(* a function that takes a while *)
fun delay n = if (n <= 0) then () else (delay(n-1); delay(n-1));

val _ = let
      val (_, t) = Time.timeToEval (fn () => delay 26)
      in
	Print.print(Time.toString t ^ " seconds\n")
      end
